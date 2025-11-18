// Copyright 2011 Google Inc. All Rights Reserved.
// Author: sreeni@google.com (Sreeni Viswanadha)

/* Copyright (c) 2006, Sun Microsystems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Sun Microsystems, Inc. nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.javacc.parser;

import java.io.*;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.javacc.utils.OutputFileGenerator;

import static org.javacc.parser.JavaCCGlobals.*;

/**
 * Generate lexer.
 */
public class LexGenC extends LexGen implements JavaCCParserConstants //CodeGenerator implements JavaCCParserConstants
{
  @Override
  void PrintClassHead()
  {
    int i, j;

    List<String> tn = new ArrayList<String>(toolNames);
    tn.add(toolName);

    switchToStaticsFile();

    switchToIncludeFile();
    genCodeLine("#include <stdio.h>");
    genCodeLine("#include <stdlib.h>");
    genCodeLine("#include <string.h>");
    genCodeLine("#include <stdbool.h>");
    genCodeLine("#include \"JavaCC.h\"");
    genCodeLine("#include \"CharStream.h\"");
    genCodeLine("#include \"Token.h\"");
    genCodeLine("#include \"ErrorHandler.h\"");
    genCodeLine("#include \"TokenManager.h\"");
    genCodeLine("#include \"" + cu_name + "Constants.h\"");

    if (Options.stringValue(Options.USEROPTION__C_TOKEN_MANAGER_INCLUDE).length() > 0) {
      genCodeLine("#include \"" + Options.stringValue(Options.USEROPTION__C_TOKEN_MANAGER_INCLUDE) + "\"\n");
    }

    genCodeLine("");

    int l = 0, kind;
    i = 1;

    genCodeLine("");
    genCodeLine("/** Token Manager. */");

    switchToIncludeFile();
    genCodeLine("typedef struct " + tokMgrClassName + " {");
    genCodeLine("  TokenManager base;");
    if (Options.getTokenManagerUsesParser()) {
        genCodeLine("  void* parser;");
    }
    genCodeLine("  CharStream *input_stream;");
    genCodeLine("  FILE* debugStream;");

    genCodeLine("  int curLexState;");
    genCodeLine("  int jjnewStateCnt;");
    genCodeLine("  int jjround;");
    genCodeLine("  int jjmatchedPos;");
    genCodeLine("  int jjmatchedKind;");
    genCodeLine("  int* jjstateSet;");
    genCodeLine("  int* jjrounds;");
    genCodeLine("  JJChar curChar;");

    genCodeLine("  JJString image;");

    genCodeLine("} " + tokMgrClassName + ";");

    switchToMainFile();

    if (token_mgr_decls != null && token_mgr_decls.size() > 0)
    {
      Token t = (Token)token_mgr_decls.get(0);
      boolean commonTokenActionSeen = false;
      boolean commonTokenActionNeeded = Options.getCommonTokenAction();

      printTokenSetup((Token)token_mgr_decls.get(0));
      ccol = 1;

      switchToMainFile();
      for (j = 0; j < token_mgr_decls.size(); j++)
      {
        t = (Token)token_mgr_decls.get(j);
        if (t.kind == IDENTIFIER &&
            commonTokenActionNeeded &&
            !commonTokenActionSeen) {
          commonTokenActionSeen = t.image.equals("CommonTokenAction");
        }

        printToken(t);
      }

      switchToIncludeFile();
      genCodeLine("  void CommonTokenAction(Token* token);");

      if (Options.getTokenManagerUsesParser()) {
        genCodeLine("  void " + tokMgrClassName + "_setParser(" + tokMgrClassName + "* self, void* parser);");
      }
      genCodeLine("");

      if (commonTokenActionNeeded && !commonTokenActionSeen)
        JavaCCErrors.warning("You have the COMMON_TOKEN_ACTION option set. " +
            "But it appears you have not defined the method :\n"+
            "      " + staticString + "void CommonTokenAction(Token *t)\n" +
        "in your TOKEN_MGR_DECLS. The generated token manager will not compile.");

    }
    else if (Options.getCommonTokenAction())
    {
      JavaCCErrors.warning("You have the COMMON_TOKEN_ACTION option set. " +
          "But you have not defined the method :\n"+
          "      " + staticString + "void CommonTokenAction(Token *t)\n" +
      "in your TOKEN_MGR_DECLS. The generated token manager will not compile.");
    }

    switchToIncludeFile();
    genCodeLine("  void " + tokMgrClassName + "_setDebugStream(" + tokMgrClassName + "* self, FILE *ds);");

    switchToMainFile();
    genCodeLine("void " + tokMgrClassName + "_setDebugStream(" + tokMgrClassName + "* self, FILE *ds)");
    genCodeLine("{ self->debugStream = ds; }");

    if(Options.getTokenManagerUsesParser()){
        genCodeLine("void " + tokMgrClassName + "_setParser(" + tokMgrClassName + "* self, void* parser) {");
        genCodeLine("    self->parser = (" + cu_name + "*) parser;");
        genCodeLine("}");
    }
  }

  void DumpDebugMethods() throws IOException
  {
    writeTemplate("/templates/c/DumpDebugMethods.template",
          "maxOrdinal", maxOrdinal,
          "stateSetSize", stateSetSize);
  }

  static void BuildLexStatesTable()
  {
    Iterator<TokenProduction> it = rexprlist.iterator();
    TokenProduction tp;
    int i;

    String[] tmpLexStateName = new String[lexstate_I2S.size()];
    while (it.hasNext())
    {
      tp = it.next();
      List<RegExprSpec> respecs = tp.respecs;
      List<TokenProduction> tps;

      for (i = 0; i < tp.lexStates.length; i++)
      {
        if ((tps = (List<TokenProduction>)allTpsForState.get(tp.lexStates[i])) == null)
        {
          tmpLexStateName[maxLexStates++] = tp.lexStates[i];
          allTpsForState.put(tp.lexStates[i], tps = new ArrayList());
        }

        tps.add(tp);
      }

      if (respecs == null || respecs.size() == 0)
        continue;

      RegularExpression re;
      for (i = 0; i < respecs.size(); i++)
        if (maxOrdinal <= (re = ((RegExprSpec)respecs.get(i)).rexp).ordinal)
          maxOrdinal = re.ordinal + 1;
    }

    kinds = new int[maxOrdinal];
    toSkip = new long[maxOrdinal / 64 + 1];
    toSpecial = new long[maxOrdinal / 64 + 1];
    toMore = new long[maxOrdinal / 64 + 1];
    toToken = new long[maxOrdinal / 64 + 1];
    toToken[0] = 1L;
    actions = new Action[maxOrdinal];
    actions[0] = actForEof;
    hasTokenActions = actForEof != null;
    initStates = new Hashtable();
    canMatchAnyChar = new int[maxLexStates];
    canLoop = new boolean[maxLexStates];
    stateHasActions = new boolean[maxLexStates];
    lexStateName = new String[maxLexStates];
    singlesToSkip = new NfaState[maxLexStates];
    System.arraycopy(tmpLexStateName, 0, lexStateName, 0, maxLexStates);

    for (i = 0; i < maxLexStates; i++)
      canMatchAnyChar[i] = -1;

    hasNfa = new boolean[maxLexStates];
    mixed = new boolean[maxLexStates];
    maxLongsReqd = new int[maxLexStates];
    initMatch = new int[maxLexStates];
    newLexState = new String[maxOrdinal];
    newLexState[0] = nextStateForEof;
    hasEmptyMatch = false;
    lexStates = new int[maxOrdinal];
    ignoreCase = new boolean[maxOrdinal];
    rexprs = new RegularExpression[maxOrdinal];
    RStringLiteral.allImages = new String[maxOrdinal];
    canReachOnMore = new boolean[maxLexStates];
  }

  static int GetIndex(String name)
  {
    for (int i = 0; i < lexStateName.length; i++)
      if (lexStateName[i] != null && lexStateName[i].equals(name))
        return i;

    throw new Error(); // Should never come here
  }

  public static void AddCharToSkip(char c, int kind)
  {
    singlesToSkip[lexStateIndex].AddChar(c);
    singlesToSkip[lexStateIndex].kind = kind;
  }

  public void start() throws IOException
  {
    if (!Options.getBuildTokenManager() ||
        Options.getUserTokenManager() ||
        JavaCCErrors.get_error_count() > 0)
      return;

    keepLineCol = Options.getKeepLineColumn();
    List choices = new ArrayList();
    Enumeration e;
    TokenProduction tp;
    int i, j;

    staticString = (Options.getStatic() ? "static " : "");
    tokMgrClassName = cu_name + "TokenManager";

    PrintClassHead();
    BuildLexStatesTable();

    e = allTpsForState.keys();

    boolean ignoring = false;

    while (e.hasMoreElements())
    {
      NfaState.ReInit();
      RStringLiteral.ReInit();

      String key = (String)e.nextElement();

      lexStateIndex = GetIndex(key);
      lexStateSuffix = "_" + lexStateIndex;
      List<TokenProduction> allTps = (List<TokenProduction>)allTpsForState.get(key);
      initStates.put(key, initialState = new NfaState());
      ignoring = false;

      singlesToSkip[lexStateIndex] = new NfaState();
      singlesToSkip[lexStateIndex].dummy = true;

      if (key.equals("DEFAULT"))
        defaultLexState = lexStateIndex;

      for (i = 0; i < allTps.size(); i++)
      {
        tp = (TokenProduction)allTps.get(i);
        int kind = tp.kind;
        boolean ignore = tp.ignoreCase;
        List<RegExprSpec> rexps = tp.respecs;

        if (i == 0)
          ignoring = ignore;

        for (j = 0; j < rexps.size(); j++)
        {
          RegExprSpec respec = (RegExprSpec)rexps.get(j);
          curRE = respec.rexp;

          rexprs[curKind = curRE.ordinal] = curRE;
          lexStates[curRE.ordinal] = lexStateIndex;
          ignoreCase[curRE.ordinal] = ignore;

          if (curRE.private_rexp)
          {
            kinds[curRE.ordinal] = -1;
            continue;
          }

          if (curRE instanceof RStringLiteral &&
              !((RStringLiteral)curRE).image.equals(""))
          {
            ((RStringLiteral)curRE).GenerateDfa(this, curRE.ordinal);
            if (i != 0 && !mixed[lexStateIndex] && ignoring != ignore)
              mixed[lexStateIndex] = true;
          }
          else if (curRE.CanMatchAnyChar())
          {
            if (canMatchAnyChar[lexStateIndex] == -1 ||
                canMatchAnyChar[lexStateIndex] > curRE.ordinal)
              canMatchAnyChar[lexStateIndex] = curRE.ordinal;
          }
          else
          {
            Nfa temp;

            if (curRE instanceof RChoice)
              choices.add(curRE);

            temp = curRE.GenerateNfa(ignore);
            temp.end.isFinal = true;
            temp.end.kind = curRE.ordinal;
            initialState.AddMove(temp.start);
          }

          if (kinds.length < curRE.ordinal)
          {
            int[] tmp = new int[curRE.ordinal + 1];

            System.arraycopy(kinds, 0, tmp, 0, kinds.length);
            kinds = tmp;
          }

          kinds[curRE.ordinal] = kind;

          if (respec.nextState != null &&
              !respec.nextState.equals(lexStateName[lexStateIndex]))
            newLexState[curRE.ordinal] = respec.nextState;

          if (respec.act != null && respec.act.getActionTokens() != null &&
              respec.act.getActionTokens().size() > 0)
            actions[curRE.ordinal] = respec.act;

          switch(kind)
          {
          case TokenProduction.SPECIAL :
            hasSkipActions |= (actions[curRE.ordinal] != null) ||
            (newLexState[curRE.ordinal] != null);
            hasSpecial = true;
            toSpecial[curRE.ordinal / 64] |= 1L << (curRE.ordinal % 64);
            toSkip[curRE.ordinal / 64] |= 1L << (curRE.ordinal % 64);
            break;
          case TokenProduction.SKIP :
            hasSkipActions |= (actions[curRE.ordinal] != null);
            hasSkip = true;
            toSkip[curRE.ordinal / 64] |= 1L << (curRE.ordinal % 64);
            break;
          case TokenProduction.MORE :
            hasMoreActions |= (actions[curRE.ordinal] != null);
            hasMore = true;
            toMore[curRE.ordinal / 64] |= 1L << (curRE.ordinal % 64);

            if (newLexState[curRE.ordinal] != null)
              canReachOnMore[GetIndex(newLexState[curRE.ordinal])] = true;
            else
              canReachOnMore[lexStateIndex] = true;

            break;
          case TokenProduction.TOKEN :
            hasTokenActions |= (actions[curRE.ordinal] != null);
            toToken[curRE.ordinal / 64] |= 1L << (curRE.ordinal % 64);
            break;
          }
        }
      }

      NfaState.ComputeClosures();

      for (i = 0; i < initialState.epsilonMoves.size(); i++)
        ((NfaState)initialState.epsilonMoves.elementAt(i)).GenerateCode();

      hasNfa[lexStateIndex] = (NfaState.generatedStates != 0);
      if (hasNfa[lexStateIndex])
      {
        initialState.GenerateCode();
        initialState.GenerateInitMoves(this);
      }

      if (initialState.kind != Integer.MAX_VALUE && initialState.kind != 0)
      {
        if ((toSkip[initialState.kind / 64] & (1L << initialState.kind)) != 0L ||
            (toSpecial[initialState.kind / 64] & (1L << initialState.kind)) != 0L)
          hasSkipActions = true;
        else if ((toMore[initialState.kind / 64] & (1L << initialState.kind)) != 0L)
          hasMoreActions = true;
        else
          hasTokenActions = true;

        if (initMatch[lexStateIndex] == 0 ||
            initMatch[lexStateIndex] > initialState.kind)
        {
          initMatch[lexStateIndex] = initialState.kind;
          hasEmptyMatch = true;
        }
      }
      else if (initMatch[lexStateIndex] == 0)
        initMatch[lexStateIndex] = Integer.MAX_VALUE;

      RStringLiteral.FillSubString();

      if (hasNfa[lexStateIndex] && !mixed[lexStateIndex])
        RStringLiteral.GenerateNfaStartStates(this, initialState);

      RStringLiteral.DumpDfaCode(this);

      if (hasNfa[lexStateIndex])
        NfaState.DumpMoveNfa(this);

      if (stateSetSize < NfaState.generatedStates)
        stateSetSize = NfaState.generatedStates;
    }

    for (i = 0; i < choices.size(); i++)
      ((RChoice)choices.get(i)).CheckUnmatchability();

    NfaState.DumpStateSets(this);
    CheckEmptyStringMatch();
    NfaState.DumpNonAsciiMoveMethods(this);
    RStringLiteral.DumpStrLiteralImages(this);
    DumpFillToken();
    DumpGetNextToken();

    if (Options.getDebugTokenManager())
    {
      NfaState.DumpStatesForKind(this);
      DumpDebugMethods();
    }

    if (hasLoop)
    {
      switchToStaticsFile();
      genCodeLine("static int  jjemptyLineNo[" + maxLexStates + "];");
      genCodeLine("static int  jjemptyColNo[" + maxLexStates + "];");
      genCodeLine("static bool jjbeenHere[" + maxLexStates + "];");
      switchToMainFile();
    }

    if (hasSkipActions)
      DumpSkipActions();
    if (hasMoreActions)
      DumpMoreActions();
    if (hasTokenActions)
      DumpTokenActions();

    NfaState.PrintBoilerPlateC(this);

    String charStreamName;
    if (Options.getUserCharStream())
      charStreamName = "CharStream";
    else
    {
      if (Options.getJavaUnicodeEscape())
        charStreamName = "JavaCharStream";
      else
        charStreamName = "SimpleCharStream";
    }
    
    switchToMainFile();
    writeTemplate("/templates/c/TokenManagerBoilerPlateMethods.template",
      "charStreamName", "CharStream",
      "parserClassName", cu_name,
      "defaultLexState", "defaultLexState",
      "lexStateNameLength", lexStateName.length,
      "tokMgrClassName", tokMgrClassName
      );

    genCodeLine("");
    genCodeLine("void " + tokMgrClassName + "_ReInitRounds(" + tokMgrClassName + "* self) {");
    genCodeLine("    int i;");
    genCodeLine("    self->jjround = 0x80000001;");
    genCodeLine("    for (i = " + NfaState.generatedStates + "; i-- > 0;)");
    genCodeLine("        self->jjrounds[i] = 0x80000000;");
    genCodeLine("}");
    genCodeLine("");
    
    genCodeLine("void lexicalError(int EOFSeen, int lexState, int errorLine, int errorColumn, JJString errorAfter, JJChar curChar) {");
    genCodeLine("    fprintf(stderr, \"Lexical error at line %d, column %d.\\n\", errorLine, errorColumn);");
    genCodeLine("    exit(1);");
    genCodeLine("}");
    genCodeLine("");
    
    genCodeLine("static Token* " + tokMgrClassName + "_getNextToken_wrapper(TokenManager* tm) {");
    genCodeLine("    return " + tokMgrClassName + "_getNextToken((" + tokMgrClassName + "*)tm);");
    genCodeLine("}");
    genCodeLine("");
    genCodeLine("static void " + tokMgrClassName + "_delete_wrapper(TokenManager* tm) {");
    genCodeLine("    " + tokMgrClassName + "* self = (" + tokMgrClassName + "*)tm;");
    genCodeLine("    if (self) {");
    genCodeLine("        free(self->jjstateSet);");
    genCodeLine("        free(self->jjrounds);");
    genCodeLine("        free(self);");
    genCodeLine("    }");
    genCodeLine("}");
    genCodeLine("");
    genCodeLine(tokMgrClassName + "* new" + tokMgrClassName + "(CharStream* stream) {");
    genCodeLine("    " + tokMgrClassName + "* self = (" + tokMgrClassName + "*)malloc(sizeof(" + tokMgrClassName + "));");
    genCodeLine("    if (!self) return NULL;");
    genCodeLine("    ");
    genCodeLine("    self->base.getNextToken = " + tokMgrClassName + "_getNextToken_wrapper;");
    genCodeLine("    self->base._delete = " + tokMgrClassName + "_delete_wrapper;");
    genCodeLine("    self->base.app_data = NULL;");
    genCodeLine("    ");
    genCodeLine("    self->input_stream = stream;");
    genCodeLine("    self->curLexState = 0;");
    genCodeLine("    self->jjnewStateCnt = 0;");
    genCodeLine("    self->jjround = 1;");
    genCodeLine("    self->jjmatchedPos = 0;");
    genCodeLine("    self->jjmatchedKind = 0;");
    genCodeLine("    self->debugStream = stdout;");
    genCodeLine("    self->curChar = 0;");
    genCodeLine("    self->jjstateSet = (int*)calloc(" + NfaState.generatedStates + ", sizeof(int));");
    genCodeLine("    self->jjrounds = (int*)calloc(" + NfaState.generatedStates + ", sizeof(int));");
    genCodeLine("    if (!self->jjstateSet || !self->jjrounds) {");
    genCodeLine("        free(self->jjstateSet);");
    genCodeLine("        free(self->jjrounds);");
    genCodeLine("        free(self);");
    genCodeLine("        return NULL;");
    genCodeLine("    }");
    genCodeLine("    " + tokMgrClassName + "_ReInitRounds(self);");
    genCodeLine("    return self;");
    genCodeLine("}");
    genCodeLine("");
    
    genCodeLine("ErrorHandler* newErrorHandler() {");
    genCodeLine("    return NULL;");
    genCodeLine("}");
    genCodeLine("");

    dumpBoilerPlateInHeader();

    DumpStaticVarDeclarations();

    switchToIncludeFile();
    writeTemplate("/templates/c/DumpVarDeclarations.template",
      "charStreamName", "CharStream",
      "lexStateNameLength", lexStateName.length,
      "tokMgrClassName", tokMgrClassName
      );

    switchToStaticsFile();
    String fileName = Options.getOutputDirectory() + File.separator +
                      tokMgrClassName +
                      getFileExtension(Options.getOutputLanguage());
    saveOutput(fileName);
  }

  private void dumpBoilerPlateInHeader() {
    switchToIncludeFile();
    genCodeLine("#ifndef JAVACC_CHARSTREAM");
    genCodeLine("#define JAVACC_CHARSTREAM CharStream");
    genCodeLine("#endif");
    genCodeLine("");

    genCodeLine("void " + tokMgrClassName + "_ReInitRounds(" + tokMgrClassName + "* self);");
    genCodeLine("");
    genCodeLine(tokMgrClassName + "* new" + tokMgrClassName + "(JAVACC_CHARSTREAM *stream);");
    genCodeLine("void delete" + tokMgrClassName + "(" + tokMgrClassName + "* self);");
    genCodeLine("void " + tokMgrClassName + "_ReInit(" + tokMgrClassName + "* self, JAVACC_CHARSTREAM *stream);");
    genCodeLine("void " + tokMgrClassName + "_SwitchTo(" + tokMgrClassName + "* self, int lexState);");
    genCodeLine("void " + tokMgrClassName + "_clear(" + tokMgrClassName + "* self);");
    genCodeLine("const JJSimpleString " + tokMgrClassName + "_jjKindsForBitVector(int i, " + Options.getLongType() + " vec);");
    genCodeLine("const JJSimpleString " + tokMgrClassName + "_jjKindsForStateVector(int lexState, int vec[], int start, int end);");
    genCodeLine("Token * " + tokMgrClassName + "_getNextToken(" + tokMgrClassName + "* self);");

    genCodeLine("");
  }

  private void DumpStaticVarDeclarations() throws IOException
  {
    int i;

    switchToStaticsFile();
    genCodeLine("");
    genCodeLine("/** Lexer state names. */");
    genStringLiteralArray("lexStateNames", lexStateName);

    if (maxLexStates > 1)
    {
      genCodeLine("");
      genCodeLine("/** Lex State array. */");
      genCode("static const int jjnewLexState[] = {");

      for (i = 0; i < maxOrdinal; i++)
      {
        if (i % 25 == 0)
          genCode("\n   ");

        if (newLexState[i] == null)
          genCode("-1, ");
        else
          genCode(GetIndex(newLexState[i]) + ", ");
      }
      genCodeLine("\n};");
    }

    if (hasSkip || hasMore || hasSpecial)
    {
      genCode("static const " + Options.getLongType() + " jjtoToken[] = {");
      for (i = 0; i < maxOrdinal / 64 + 1; i++)
      {
        if (i % 4 == 0)
          genCode("\n   ");
        genCode("0x" + Long.toHexString(toToken[i]) + "L, ");
      }
      genCodeLine("\n};");
    }

    if (hasSkip || hasSpecial)
    {
      genCode("static const " + Options.getLongType() + " jjtoSkip[] = {");
      for (i = 0; i < maxOrdinal / 64 + 1; i++)
      {
        if (i % 4 == 0)
          genCode("\n   ");
        genCode("0x" + Long.toHexString(toSkip[i]) + "L, ");
      }
      genCodeLine("\n};");
    }

    if (hasSpecial)
    {
      genCode("static const " + Options.getLongType() + " jjtoSpecial[] = {");
      for (i = 0; i < maxOrdinal / 64 + 1; i++)
      {
        if (i % 4 == 0)
          genCode("\n   ");
        genCode("0x" + Long.toHexString(toSpecial[i]) + "L, ");
      }
      genCodeLine("\n};");
    }
  }

  void DumpFillToken()
  {
    final double tokenVersion = JavaFiles.getVersion("Token.java");
    final boolean hasBinaryNewToken = tokenVersion > 4.09;

    genCodeLine("static Token * jjFillToken(" + tokMgrClassName + "* self)");
    genCodeLine("{");
    genCodeLine("   Token *t;");
    genCodeLine("   JJString curTokenImage;");
    if (keepLineCol)
    {
      genCodeLine("   int beginLine   = -1;");
      genCodeLine("   int endLine     = -1;");
      genCodeLine("   int beginColumn = -1;");
      genCodeLine("   int endColumn   = -1;");
    }

    if (hasEmptyMatch)
    {
      genCodeLine("   if (self->jjmatchedPos < 0)");
      genCodeLine("   {");
      genCodeLine("       curTokenImage = JJString_FROM_CHAR_ARRAY(self->image.str, self->image.len);");

      if (keepLineCol)
      {
        genCodeLine("   if (self->input_stream->getTrackLineColumn(self->input_stream)) {");
        genCodeLine("      beginLine = endLine = self->input_stream->getEndLine(self->input_stream);");
        genCodeLine("      beginColumn = endColumn = self->input_stream->getEndColumn(self->input_stream);");
        genCodeLine("   }");
      }

      genCodeLine("   }");
      genCodeLine("   else");
      genCodeLine("   {");
      genCodeLine("      JJString im = jjstrLiteralImages[self->jjmatchedKind];");
      genCodeLine("      curTokenImage = (im.len == 0) ? self->input_stream->GetImage(self->input_stream) : im;");

      if (keepLineCol)
      {
        genCodeLine("   if (self->input_stream->getTrackLineColumn(self->input_stream)) {");
        genCodeLine("      beginLine = self->input_stream->getBeginLine(self->input_stream);");
        genCodeLine("      beginColumn = self->input_stream->getBeginColumn(self->input_stream);");
        genCodeLine("      endLine = self->input_stream->getEndLine(self->input_stream);");
        genCodeLine("      endColumn = self->input_stream->getEndColumn(self->input_stream);");
        genCodeLine("   }");
      }

      genCodeLine("   }");
    }
    else
    {
      genCodeLine("   JJString im = jjstrLiteralImages[self->jjmatchedKind];");
      genCodeLine("   curTokenImage = (im.len == 0) ? self->input_stream->GetImage(self->input_stream) : im;");
      if (keepLineCol)
      {
        genCodeLine("   if (self->input_stream->getTrackLineColumn(self->input_stream)) {");
        genCodeLine("     beginLine = self->input_stream->getBeginLine(self->input_stream);");
        genCodeLine("     beginColumn = self->input_stream->getBeginColumn(self->input_stream);");
        genCodeLine("     endLine = self->input_stream->getEndLine(self->input_stream);");
        genCodeLine("     endColumn = self->input_stream->getEndColumn(self->input_stream);");
        genCodeLine("   }");
      }
    }

    if (Options.getTokenFactory().length() > 0) {
      genCodeLine("   t = " + getClassQualifier(Options.getTokenFactory()) + "newToken(self->jjmatchedKind, curTokenImage);");
    } else {
      genCodeLine("   t = newToken(self->jjmatchedKind, curTokenImage);");
    }

    genCodeLine("   t->specialToken = NULL;");
    genCodeLine("   t->next = NULL;");

    if (keepLineCol) {
      genCodeLine("");
      genCodeLine("   if (self->input_stream->getTrackLineColumn(self->input_stream)) {");
      genCodeLine("   t->beginLine = beginLine;");
      genCodeLine("   t->endLine = endLine;");
      genCodeLine("   t->beginColumn = beginColumn;");
      genCodeLine("   t->endColumn = endColumn;");
      genCodeLine("   }");
    }

    genCodeLine("");
    genCodeLine("   return t;");
    genCodeLine("}");
  }

  void DumpGetNextToken()
  {
    int i;

    switchToMainFile();
    genCodeLine("/** Get the next Token. */");
    genCodeLine("Token * " + tokMgrClassName + "_getNextToken(" + tokMgrClassName + "* self)");
    genCodeLine("{");
    if (hasSpecial) {
      genCodeLine("  Token *specialToken = NULL;");
    }
    genCodeLine("  Token *matchedToken = NULL;");
    genCodeLine("  int curPos = 0;");
    genCodeLine("  JJChar curChar;");

    genCodeLine("");
    genCodeLine("  for (;;)");
    genCodeLine("  {");
    genCodeLine("   EOFLoop: ");
    genCodeLine("   if (self->input_stream->endOfInput(self->input_stream))");
    genCodeLine("   {");

    if (Options.getDebugTokenManager())
      genCodeLine("      fprintf(self->debugStream, \"Returning the <EOF> token.\\n\");");

    genCodeLine("      self->jjmatchedKind = 0;");
    genCodeLine("      self->jjmatchedPos = -1;");
    genCodeLine("      matchedToken = jjFillToken(self);");

    if (hasSpecial)
      genCodeLine("      matchedToken->specialToken = specialToken;");

    if (nextStateForEof != null || actForEof != null)
      genCodeLine("      TokenLexicalActions(matchedToken);");

    if (Options.getCommonTokenAction())
      genCodeLine("      CommonTokenAction(matchedToken);");

    genCodeLine("      return matchedToken;");
    genCodeLine("   }");
    genCodeLine("   curChar = self->input_stream->BeginToken(self->input_stream);");

    if (hasMoreActions || hasSkipActions || hasTokenActions)
    {
      genCodeLine("   JJString_clear(&self->image);");
      genCodeLine("   jjimageLen = 0;");
    }

    genCodeLine("");

    String prefix = "";
    if (hasMore)
    {
      genCodeLine("   for (;;)");
      genCodeLine("   {");
      prefix = "  ";
    }

    String endSwitch = "";
    String caseStr = "";
    if (maxLexStates > 1)
    {
      genCodeLine(prefix + "   switch(self->curLexState)");
      genCodeLine(prefix + "   {");
      endSwitch = prefix + "   }";
      caseStr = prefix + "     case ";
      prefix += "    ";
    }

    prefix += "   ";
    for(i = 0; i < maxLexStates; i++)
    {
      if (maxLexStates > 1)
        genCodeLine(caseStr + i + ":");

      if (singlesToSkip[i].HasTransitions())
      {
        genCodeLine(prefix + "{ self->input_stream->backup(self->input_stream, 0);");
        if (singlesToSkip[i].asciiMoves[0] != 0L &&
            singlesToSkip[i].asciiMoves[1] != 0L)
        {
          genCodeLine(prefix + "   while ((curChar < 64" + " && (0x" +
              Long.toHexString(singlesToSkip[i].asciiMoves[0]) +
              "L & (1L << curChar)) != 0L) || \n" +
              prefix + "          (curChar >> 6) == 1" +
              " && (0x" +
              Long.toHexString(singlesToSkip[i].asciiMoves[1]) +
          "L & (1L << (curChar & 077))) != 0L)");
        }
        else if (singlesToSkip[i].asciiMoves[1] == 0L)
        {
          genCodeLine(prefix + "   while (curChar <= " +
              (int)MaxChar(singlesToSkip[i].asciiMoves[0]) + " && (0x" +
              Long.toHexString(singlesToSkip[i].asciiMoves[0]) +
          "L & (1L << curChar)) != 0L)");
        }
        else if (singlesToSkip[i].asciiMoves[0] == 0L)
        {
          genCodeLine(prefix + "   while (curChar > 63 && curChar <= " +
              ((int)MaxChar(singlesToSkip[i].asciiMoves[1]) + 64) +
              " && (0x" +
              Long.toHexString(singlesToSkip[i].asciiMoves[1]) +
          "L & (1L << (curChar & 077))) != 0L)");
        }

        genCodeLine(prefix + "{");
        if (Options.getDebugTokenManager())
        {
          if (maxLexStates > 1) {
            genCodeLine("      fprintf(self->debugStream, \"<%s>\" , lexStateNames[self->curLexState]);");
          }

          genCodeLine("      fprintf(self->debugStream, \"Skipping character : %c(%d)\\n\", curChar, (int)curChar);");
        }

        genCodeLine(prefix + "if (self->input_stream->endOfInput(self->input_stream)) { goto EOFLoop; }");
        genCodeLine(prefix + "curChar = self->input_stream->BeginToken(self->input_stream);");
        genCodeLine(prefix + "}");
        genCodeLine(prefix + "}");
      }

      if (initMatch[i] != Integer.MAX_VALUE && initMatch[i] != 0)
      {
        if (Options.getDebugTokenManager())
          genCodeLine("      fprintf(self->debugStream, \"   Matched the empty string as %s token.\\n\", tokenImage[" + initMatch[i] + "]);");

        genCodeLine(prefix + "self->jjmatchedKind = " + initMatch[i] + ";");
        genCodeLine(prefix + "self->jjmatchedPos = -1;");
        genCodeLine(prefix + "curPos = 0;");
      }
      else
      {
        genCodeLine(prefix + "self->jjmatchedKind = 0x" + Integer.toHexString(Integer.MAX_VALUE) + ";");
        genCodeLine(prefix + "self->jjmatchedPos = 0;");
      }

      if (Options.getDebugTokenManager()) {
        genCodeLine("   fprintf(self->debugStream, " +
          "\"<%s>Current character : %c(%d) at line %d column %d\\n\","+
          "lexStateNames[self->curLexState], curChar, (int)curChar, " +
          "self->input_stream->getEndLine(self->input_stream), self->input_stream->getEndColumn(self->input_stream));");
      }

      genCodeLine(prefix + "curPos = jjMoveStringLiteralDfa0_" + i + "(self, curChar);");

      if (canMatchAnyChar[i] != -1)
      {
        if (initMatch[i] != Integer.MAX_VALUE && initMatch[i] != 0)
          genCodeLine(prefix + "if (self->jjmatchedPos < 0 || (self->jjmatchedPos == 0 && self->jjmatchedKind > " +
              canMatchAnyChar[i] + "))");
        else
          genCodeLine(prefix + "if (self->jjmatchedPos == 0 && self->jjmatchedKind > " +
              canMatchAnyChar[i] + ")");
        genCodeLine(prefix + "{");

        if (Options.getDebugTokenManager()) {
          genCodeLine("           fprintf(self->debugStream, \"   Current character matched as a %s token.\\n\", tokenImage[" + canMatchAnyChar[i] + "]);");
        }
        genCodeLine(prefix + "   self->jjmatchedKind = " + canMatchAnyChar[i] + ";");

        if (initMatch[i] != Integer.MAX_VALUE && initMatch[i] != 0)
          genCodeLine(prefix + "   self->jjmatchedPos = 0;");

        genCodeLine(prefix + "}");
      }

      if (maxLexStates > 1)
        genCodeLine(prefix + "break;");
    }

    if (maxLexStates > 1)
      genCodeLine(endSwitch);
    else if (maxLexStates == 0)
      genCodeLine("       self->jjmatchedKind = 0x" + Integer.toHexString(Integer.MAX_VALUE) + ";");

    if (maxLexStates > 1)
      prefix = "  ";
    else
      prefix = "";

    if (maxLexStates > 0)
    {
      genCodeLine(prefix + "   if (self->jjmatchedKind != 0x" + Integer.toHexString(Integer.MAX_VALUE) + ")");
      genCodeLine(prefix + "   {");
      genCodeLine(prefix + "      if (self->jjmatchedPos + 1 < curPos)");

      if (Options.getDebugTokenManager()) {
        genCodeLine(prefix + "      {");
        genCodeLine(prefix + "         fprintf(self->debugStream, " +
        "\"   Putting back %d characters into the input stream.\\n\", (curPos - self->jjmatchedPos - 1));");
      }

      genCodeLine(prefix + "         self->input_stream->backup(self->input_stream, curPos - self->jjmatchedPos - 1);");

      if (Options.getDebugTokenManager()) {
        genCodeLine(prefix + "      }");
      }

      if (Options.getDebugTokenManager())
      {
          genCodeLine("    fprintf(self->debugStream, " +
              "\"****** FOUND A %d(%s) MATCH (%s) ******\\n\", self->jjmatchedKind, tokenImage[self->jjmatchedKind], self->input_stream->GetSuffix(self->input_stream, self->jjmatchedPos + 1).str);");
      }

      if (hasSkip || hasMore || hasSpecial)
      {
        genCodeLine(prefix + "      if ((jjtoToken[self->jjmatchedKind >> 6] & " +
        "(1L << (self->jjmatchedKind & 077))) != 0L)");
        genCodeLine(prefix + "      {");
      }

      genCodeLine(prefix + "         matchedToken = jjFillToken(self);");

      if (hasSpecial)
        genCodeLine(prefix + "         matchedToken->specialToken = specialToken;");

      if (hasTokenActions)
        genCodeLine(prefix + "         TokenLexicalActions(matchedToken);");

      if (maxLexStates > 1)
      {
        genCodeLine("       if (jjnewLexState[self->jjmatchedKind] != -1)");
        genCodeLine(prefix + "       self->curLexState = jjnewLexState[self->jjmatchedKind];");
      }

      if (Options.getCommonTokenAction())
        genCodeLine(prefix + "         CommonTokenAction(matchedToken);");

      genCodeLine(prefix + "         return matchedToken;");

      if (hasSkip || hasMore || hasSpecial)
      {
        genCodeLine(prefix + "      }");

        if (hasSkip || hasSpecial)
        {
          if (hasMore)
          {
            genCodeLine(prefix + "      else if ((jjtoSkip[self->jjmatchedKind >> 6] & " +
            "(1L << (self->jjmatchedKind & 077))) != 0L)");
          }
          else
            genCodeLine(prefix + "      else");

          genCodeLine(prefix + "      {");

          if (hasSpecial)
          {
            genCodeLine(prefix + "         if ((jjtoSpecial[self->jjmatchedKind >> 6] & " +
            "(1L << (self->jjmatchedKind & 077))) != 0L)");
            genCodeLine(prefix + "         {");

            genCodeLine(prefix + "            matchedToken = jjFillToken(self);");

            genCodeLine(prefix + "            if (specialToken == NULL)");
            genCodeLine(prefix + "               specialToken = matchedToken;");
            genCodeLine(prefix + "            else");
            genCodeLine(prefix + "            {");
            genCodeLine(prefix + "               matchedToken->specialToken = specialToken;");
            genCodeLine(prefix + "               specialToken = (specialToken->next = matchedToken);");
            genCodeLine(prefix + "            }");

            if (hasSkipActions)
              genCodeLine(prefix + "            SkipLexicalActions(self, matchedToken);");

            genCodeLine(prefix + "         }");

            if (hasSkipActions)
            {
              genCodeLine(prefix + "         else");
              genCodeLine(prefix + "            SkipLexicalActions(self, NULL);");
            }
          }
          else if (hasSkipActions)
            genCodeLine(prefix + "         SkipLexicalActions(self, NULL);");

          if (maxLexStates > 1)
          {
            genCodeLine("         if (jjnewLexState[self->jjmatchedKind] != -1)");
            genCodeLine(prefix + "         self->curLexState = jjnewLexState[self->jjmatchedKind];");
          }

          genCodeLine(prefix + "         goto EOFLoop;");
          genCodeLine(prefix + "      }");
        }

        if (hasMore)
        {
          if (hasMoreActions)
            genCodeLine(prefix + "      MoreLexicalActions(self);");
          else if (hasSkipActions || hasTokenActions)
            genCodeLine(prefix + "      jjimageLen += self->jjmatchedPos + 1;");

          if (maxLexStates > 1)
          {
            genCodeLine("      if (jjnewLexState[self->jjmatchedKind] != -1)");
            genCodeLine(prefix + "      self->curLexState = jjnewLexState[self->jjmatchedKind];");
          }
          genCodeLine(prefix + "      curPos = 0;");
          genCodeLine(prefix + "      self->jjmatchedKind = 0x" + Integer.toHexString(Integer.MAX_VALUE) + ";");

          genCodeLine(prefix + "   if (!self->input_stream->endOfInput(self->input_stream)) {");
          genCodeLine(prefix + "         curChar = self->input_stream->readChar(self->input_stream);");

          if (Options.getDebugTokenManager()) {
            genCodeLine("   fprintf(self->debugStream, " +
             "\"<%s>Current character : %c(%d) at line %d column %d\\n\","+
             "lexStateNames[self->curLexState], curChar, (int)curChar, " +
             "self->input_stream->getEndLine(self->input_stream), self->input_stream->getEndColumn(self->input_stream));");
          }
          genCodeLine(prefix + "   continue;");
          genCodeLine(prefix + " }");
        }
      }

      genCodeLine(prefix + "   }");
      genCodeLine(prefix + "   int error_line = self->input_stream->getEndLine(self->input_stream);");
      genCodeLine(prefix + "   int error_column = self->input_stream->getEndColumn(self->input_stream);");
      genCodeLine(prefix + "   JJString error_after;");
      genCodeLine(prefix + "   bool EOFSeen = false;");
      genCodeLine(prefix + "   if (self->input_stream->endOfInput(self->input_stream)) {");
      genCodeLine(prefix + "      EOFSeen = true;");
      genCodeLine(prefix + "      error_after = curPos <= 1 ? JJString_FROM_CONST(\"\") : self->input_stream->GetImage(self->input_stream);");
      genCodeLine(prefix + "      if (curChar == '\\n' || curChar == '\\r') {");
      genCodeLine(prefix + "         error_line++;");
      genCodeLine(prefix + "         error_column = 0;");
      genCodeLine(prefix + "      }");
      genCodeLine(prefix + "      else");
      genCodeLine(prefix + "         error_column++;");
      genCodeLine(prefix + "   }");
      genCodeLine(prefix + "   if (!EOFSeen) {");
      genCodeLine(prefix + "      error_after = curPos <= 1 ? JJString_FROM_CONST(\"\") : self->input_stream->GetImage(self->input_stream);");
      genCodeLine(prefix + "   }");
      if (Options.getTokenManagerUsesParser()) {
        genCodeLine(prefix + "   ((ErrorHandler*) self->parser)->lexicalError(EOFSeen, self->curLexState, error_line, error_column, error_after, curChar);");
      } else {
        genCodeLine(prefix + "   lexicalError(EOFSeen, self->curLexState, error_line, error_column, error_after, curChar);");
      }
    }

    if (hasMore)
      genCodeLine(prefix + " }");

    genCodeLine("  }");
    genCodeLine("}");
    genCodeLine("");
  }

  public void DumpSkipActions()
  {
    Action act;

    genCodeLine("void SkipLexicalActions(" + tokMgrClassName + "* self, Token *matchedToken)");
    genCodeLine("{");
    genCodeLine("   switch(self->jjmatchedKind)");
    genCodeLine("   {");

    Outer:
      for (int i = 0; i < maxOrdinal; i++)
      {
        if ((toSkip[i / 64] & (1L << (i % 64))) == 0L)
          continue;

        for (;;)
        {
          if (((act = (Action)actions[i]) == null ||
              act.getActionTokens() == null ||
              act.getActionTokens().size() == 0) && !canLoop[lexStates[i]])
            continue Outer;

          genCodeLine("      case " + i + " : {");

          if (initMatch[lexStates[i]] == i && canLoop[lexStates[i]])
          {
            genCodeLine("         if (self->jjmatchedPos == -1)");
            genCodeLine("         {");
            genCodeLine("            if (jjbeenHere[" + lexStates[i] + "] &&");
            genCodeLine("                jjemptyLineNo[" + lexStates[i] + "] == self->input_stream->getBeginLine(self->input_stream) &&");
            genCodeLine("                jjemptyColNo[" + lexStates[i] + "] == self->input_stream->getBeginColumn(self->input_stream))");
            if (Options.getTokenManagerUsesParser()) {
              genCodeLine("               ((ErrorHandler*) self->parser)->lexicalError(JJString_FROM_CONST(\"Error: Bailing out of infinite loop caused by repeated empty string matches at line ...\"));");
            } else {
              genCodeLine("               lexicalError(JJString_FROM_CONST(\"Error: Bailing out of infinite loop caused by repeated empty string matches at line ...\"));");
            }
            genCodeLine("            jjemptyLineNo[" + lexStates[i] + "] = self->input_stream->getBeginLine(self->input_stream);");
            genCodeLine("            jjemptyColNo[" + lexStates[i] + "] = self->input_stream->getBeginColumn(self->input_stream);");
            genCodeLine("            jjbeenHere[" + lexStates[i] + "] = true;");
            genCodeLine("         }");
          }

          if ((act = (Action)actions[i]) == null ||
              act.getActionTokens().size() == 0)
            break;

          if (RStringLiteral.allImages[i] != null) {
            genCodeLine("        JJString_append_jjstring(&self->image, jjstrLiteralImages[" + i + "]);");
            genCodeLine("        lengthOfMatch = jjstrLiteralImages[" + i + "].len;");
          } else {
            genCodeLine("        JJString suffix = self->input_stream->GetSuffix(self->input_stream, jjimageLen + (lengthOfMatch = self->jjmatchedPos + 1));");
            genCodeLine("        JJString_append_jjstring(&self->image, suffix);");
            genCodeLine("        JJString_delete(&suffix);");
          }

          printTokenSetup((Token)act.getActionTokens().get(0));
          ccol = 1;

          for (int j = 0; j < act.getActionTokens().size(); j++)
            printToken((Token)act.getActionTokens().get(j));
          genCodeLine("");

          break;
        }

        genCodeLine("         break;");
        genCodeLine("       }");
      }

    genCodeLine("      default :");
    genCodeLine("         break;");
    genCodeLine("   }");
    genCodeLine("}");
  }

  public void DumpMoreActions()
  {
    Action act;

    genCodeLine("void MoreLexicalActions(" + tokMgrClassName + "* self)");
    genCodeLine("{");
    genCodeLine("   jjimageLen += (lengthOfMatch = self->jjmatchedPos + 1);");
    genCodeLine("   switch(self->jjmatchedKind)");
    genCodeLine("   {");

    Outer:
      for (int i = 0; i < maxOrdinal; i++)
      {
        if ((toMore[i / 64] & (1L << (i % 64))) == 0L)
          continue;

        for (;;)
        {
          if (((act = (Action)actions[i]) == null ||
              act.getActionTokens() == null ||
              act.getActionTokens().size() == 0) && !canLoop[lexStates[i]])
            continue Outer;

          genCodeLine("      case " + i + " : {");

          if (initMatch[lexStates[i]] == i && canLoop[lexStates[i]])
          {
            genCodeLine("         if (self->jjmatchedPos == -1)");
            genCodeLine("         {");
            genCodeLine("            if (jjbeenHere[" + lexStates[i] + "] &&");
            genCodeLine("                jjemptyLineNo[" + lexStates[i] + "] == self->input_stream->getBeginLine(self->input_stream) &&");
            genCodeLine("                jjemptyColNo[" + lexStates[i] + "] == self->input_stream->getBeginColumn(self->input_stream))");
            if (Options.getTokenManagerUsesParser()) {
              genCodeLine("               ((ErrorHandler*) self->parser)->lexicalError(JJString_FROM_CONST(\"Error: Bailing out of infinite loop caused by repeated empty string matches at line ...\"));");
            } else {
              genCodeLine("               lexicalError(JJString_FROM_CONST(\"Error: Bailing out of infinite loop caused by repeated empty string matches at line ...\"));");
            }
            genCodeLine("            jjemptyLineNo[" + lexStates[i] + "] = self->input_stream->getBeginLine(self->input_stream);");
            genCodeLine("            jjemptyColNo[" + lexStates[i] + "] = self->input_stream->getBeginColumn(self->input_stream);");
            genCodeLine("            jjbeenHere[" + lexStates[i] + "] = true;");
            genCodeLine("         }");
          }

          if ((act = (Action)actions[i]) == null ||
              act.getActionTokens().size() == 0)
          {
            break;
          }

          if (RStringLiteral.allImages[i] != null)
            genCodeLine("        JJString_append_jjstring(&self->image, jjstrLiteralImages[" + i + "]);");
          else {
            genCodeLine("        JJString suffix = self->input_stream->GetSuffix(self->input_stream, jjimageLen);");
            genCodeLine("        JJString_append_jjstring(&self->image, suffix);");
            genCodeLine("        JJString_delete(&suffix);");
          }

          genCodeLine("         jjimageLen = 0;");
          printTokenSetup((Token)act.getActionTokens().get(0));
          ccol = 1;

          for (int j = 0; j < act.getActionTokens().size(); j++)
            printToken((Token)act.getActionTokens().get(j));
          genCodeLine("");

          break;
        }

        genCodeLine("         break;");
        genCodeLine("       }");
      }

    genCodeLine("      default :");
    genCodeLine("         break;");

    genCodeLine("   }");
    genCodeLine("}");
  }

  public void DumpTokenActions()
  {
    Action act;
    int i;

    genCodeLine("void TokenLexicalActions(" + tokMgrClassName + "* self, Token *matchedToken)");
    genCodeLine("{");
    genCodeLine("   switch(self->jjmatchedKind)");
    genCodeLine("   {");

    Outer:
      for (i = 0; i < maxOrdinal; i++)
      {
        if ((toToken[i / 64] & (1L << (i % 64))) == 0L)
          continue;

        for (;;)
        {
          if (((act = (Action)actions[i]) == null ||
              act.getActionTokens() == null ||
              act.getActionTokens().size() == 0) && !canLoop[lexStates[i]])
            continue Outer;

          genCodeLine("      case " + i + " : {");

          if (initMatch[lexStates[i]] == i && canLoop[lexStates[i]])
          {
            genCodeLine("         if (self->jjmatchedPos == -1)");
            genCodeLine("         {");
            genCodeLine("            if (jjbeenHere[" + lexStates[i] + "] &&");
            genCodeLine("                jjemptyLineNo[" + lexStates[i] + "] == self->input_stream->getBeginLine(self->input_stream) &&");
            genCodeLine("                jjemptyColNo[" + lexStates[i] + "] == self->input_stream->getBeginColumn(self->input_stream))");
            if (Options.getTokenManagerUsesParser()) {
              genCodeLine("               ((ErrorHandler*) self->parser)->lexicalError(JJString_FROM_CONST(\"Error: Bailing out of infinite loop caused by repeated empty string matches at line ...\"));");
            } else {
              genCodeLine("               lexicalError(JJString_FROM_CONST(\"Error: Bailing out of infinite loop caused by repeated empty string matches at line ...\"));");
            }
            genCodeLine("            jjemptyLineNo[" + lexStates[i] + "] = self->input_stream->getBeginLine(self->input_stream);");
            genCodeLine("            jjemptyColNo[" + lexStates[i] + "] = self->input_stream->getBeginColumn(self->input_stream);");
            genCodeLine("            jjbeenHere[" + lexStates[i] + "] = true;");
            genCodeLine("         }");
          }

          if ((act = (Action)actions[i]) == null ||
              act.getActionTokens().size() == 0)
            break;

          if (i == 0)
          {
            genCodeLine("      JJString_clear(&self->image);");
          }
          else
          {
            if (RStringLiteral.allImages[i] != null) {
              genCodeLine("        JJString_append_jjstring(&self->image, jjstrLiteralImages[" + i + "]);");
              genCodeLine("        lengthOfMatch = jjstrLiteralImages[" + i + "].len;");
            } else {
              genCodeLine("        JJString suffix = self->input_stream->GetSuffix(self->input_stream, jjimageLen + (lengthOfMatch = self->jjmatchedPos + 1));");
              genCodeLine("        JJString_append_jjstring(&self->image, suffix);");
              genCodeLine("        JJString_delete(&suffix);");
            }
          }

          printTokenSetup((Token)act.getActionTokens().get(0));
          ccol = 1;

          for (int j = 0; j < act.getActionTokens().size(); j++)
            printToken((Token)act.getActionTokens().get(j));
          genCodeLine("");

          break;
        }

        genCodeLine("         break;");
        genCodeLine("       }");
      }

    genCodeLine("      default :");
    genCodeLine("         break;");
    genCodeLine("   }");
    genCodeLine("}");
  }
}
