// Copyright 2011 Google Inc. All Rights Reserved.
// Author: sreeni@google.com (Sreeni Viswanadha)

package org.javacc.parser;

import static org.javacc.parser.JavaCCGlobals.cu_from_insertion_point_2;
import static org.javacc.parser.JavaCCGlobals.cu_name;
import static org.javacc.parser.JavaCCGlobals.cu_to_insertion_point_2;
import static org.javacc.parser.JavaCCGlobals.getFileExtension;
import static org.javacc.parser.JavaCCGlobals.jj2index;
import static org.javacc.parser.JavaCCGlobals.jjtreeGenerated;
import static org.javacc.parser.JavaCCGlobals.lookaheadNeeded;
import static org.javacc.parser.JavaCCGlobals.maskVals;
import static org.javacc.parser.JavaCCGlobals.maskindex;
import static org.javacc.parser.JavaCCGlobals.tokenCount;
import static org.javacc.parser.JavaCCGlobals.toolName;
import static org.javacc.parser.JavaCCGlobals.toolNames;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Generate the parser.
 */
public class ParseGenC extends ParseGen {

public void start() throws MetaParseException {

    Token t = null;

    if (JavaCCErrors.get_error_count() != 0) throw new MetaParseException();

    List<String> tn = new ArrayList<String>(toolNames);
    tn.add(toolName);
    switchToStaticsFile();

    switchToIncludeFile();

    genCodeLine("#include <stdio.h>");
    genCodeLine("#include <stdlib.h>");
    genCodeLine("#include <string.h>");
    genCodeLine("#include <stdbool.h>");
    genCodeLine("#include <stddef.h>");
    
    genCodeLine("#include \"JavaCC.h\"");
    genCodeLine("#include \"CharStream.h\"");
    genCodeLine("#include \"Token.h\"");
    genCodeLine("#include \"TokenManager.h\"");


    Object object = Options.objectValue(Options.USEROPTION__C_PARSER_INCLUDE);

    if (object instanceof String) {
    	String include = (String)object;
    	if (include.length() > 0) {
    		if (include.charAt(0) == '<')
    			genCodeLine("#include " + include);
    		else
    			genCodeLine("#include \"" + include + "\"");
    	}
    }

    genCodeLine("#include \"" + cu_name + "Constants.h\"");

    if (jjtreeGenerated) {
      genCodeLine("#include \"JJT" + cu_name + "State.h\"");
    }

    genCodeLine("#include \"ErrorHandler.h\"");

    if (jjtreeGenerated) {
      genCodeLine("#include \"" + cu_name + "Tree.h\"");
    }

    switchToIncludeFile();
    genCodeLine("  struct JJCalls;");
    genCodeLine("  typedef struct JJCalls {");
    genCodeLine("    int        gen;");
    genCodeLine("    int        arg;");
    genCodeLine("    struct JJCalls*   next;");
    genCodeLine("    Token*     first;");
    genCodeLine("  } JJCalls;");
    genCodeLine("");

    switchToIncludeFile();
    String superClass = Options.stringValue(Options.USEROPTION__PARSER_SUPER_CLASS);
    genCodeLine("typedef struct " + cu_name + " {");
    
    if (superClass != null && !superClass.isEmpty()) {
      genCodeLine("  " + superClass + " superClass;");
    }
    
    genCodeLine("  void (*free)(void* ptr);");
    
    genCodeLine("  ErrorHandler *errorHandler;");
    genCodeLine("  TokenManager *token_source;");
    genCodeLine("  CharStream   *jj_input_stream;");
    genCodeLine("  Token        *token;");
    genCodeLine("  Token        *jj_nt;");
    genCodeLine("  Token        *head;");
    
    genCodeLine("  int           jj_ntk;");

    genCodeLine("  JJCalls       jj_2_rtns[" + (jj2index + 1) + "];");
    genCodeLine("  bool          jj_rescan;");
    genCodeLine("  int           jj_gc;");
    genCodeLine("  Token        *jj_scanpos, *jj_lastpos;");
    genCodeLine("  int           jj_la;");
    genCodeLine("  bool          jj_lookingAhead;");
    genCodeLine("  bool          jj_semLA;");

    genCodeLine("  int           jj_gen;");
    genCodeLine("  int           jj_la1[" + (maskindex + 1) + "];");
    
    genCodeLine("  bool          hasError;");
    
    if (Options.getDepthLimit() > 0) {
      genCodeLine("  int jj_depth;");
      genCodeLine("  bool jj_depth_error;");
    }
    if (!Options.getStackLimit().equals("")) {
      genCodeLine("  size_t jj_stack_limit;");
      genCodeLine("  void* jj_stack_base;");
      genCodeLine("  bool jj_stack_error;");
    }
    
    genCodeLine("  int  indent;");
    genCodeLine("  bool trace;");
    if (jjtreeGenerated) {
      genCodeLine("  JJT" + cu_name + "State jjtree;");
    }
    genCodeLine("  bool jj_done;");
    genCodeLine("  int jj_kind;");

    genCodeLine("} " + cu_name + ";");
    
    switchToMainFile();
    if (cu_to_insertion_point_2.size() != 0) {
      printTokenSetup((cu_to_insertion_point_2.get(0)));
      for (Iterator<?> it = cu_to_insertion_point_2.iterator(); it.hasNext();) {
        t = (Token)it.next();
        printToken(t);
      }
    }

    new ParseEngine().build(this);
    
    switchToIncludeFile();
    genCodeLine("void " + cu_name + "_setErrorHandler(" + cu_name + "* self, ErrorHandler *eh);");
    
    switchToMainFile();
    genCodeLine("void " + cu_name + "_setErrorHandler(" + cu_name + "* self, ErrorHandler *eh) {");
    genCodeLine("    if (self->errorHandler) self->free(self->errorHandler);");
    genCodeLine("    self->errorHandler = eh;");
    genCodeLine("}");

    int tokenMaskSize = (tokenCount-1)/32 + 1;
    if (Options.getErrorReporting() && tokenMaskSize > 0) {
      switchToStaticsFile();
      for (int i = 0; i < tokenMaskSize; i++) {
        if (maskVals.size() > 0) {
          genCodeLine("  static unsigned int jj_la1_" + i + "[] = {");
          for (Iterator<?> it = maskVals.iterator(); it.hasNext();) {
            int[] tokenMask = (int[])(it.next());
            genCode("0x" + Integer.toHexString(tokenMask[i]) + ",");
          }
          genCodeLine("};");
        }
      }
    }

    if (Options.getDepthLimit() > 0) {
      // Depth check logic will be inlined or done via macros in C
    }
    if (!Options.getStackLimit().equals("")) {
      switchToIncludeFile();
      genCodeLine("bool " + cu_name + "_jj_stack_check(" + cu_name + "* self, bool init);");
      switchToMainFile();
      genCodeLine("bool " + cu_name + "_jj_stack_check(" + cu_name + "* self, bool init)");
      genCodeLine("  {");
      genCodeLine("     if(init) {");
      genCodeLine("       self->jj_stack_base = NULL;");
      genCodeLine("       return false;");
      genCodeLine("     } else {");
      genCodeLine("       volatile int q = 0;");
      genCodeLine("       if(!self->jj_stack_base) {");
      genCodeLine("         self->jj_stack_base = (void*)&q;");
      genCodeLine("         return false;");
      genCodeLine("       } else {");
      genCodeLine("         ptrdiff_t used = (char*)self->jj_stack_base - (char*)&q;");
      genCodeLine("         return (abs(used) > self->jj_stack_limit);");
      genCodeLine("       }");
      genCodeLine("     }");
      genCodeLine("  }");
    }

    switchToIncludeFile();
    genCodeLine(cu_name + "* new" + cu_name + "(TokenManager *tokenManager);");
    genCodeLine("void delete" + cu_name + "(" + cu_name + "* self);");
    genCodeLine("void " + cu_name + "_ReInit(" + cu_name + "* self, TokenManager* tokenManager);");
    genCodeLine("void " + cu_name + "_clear(" + cu_name + "* self);");

    switchToMainFile();
    genCodeLine(cu_name + "* new" + cu_name + "(TokenManager *tokenManager)");
    genCodeLine("{");
    genCodeLine("    " + cu_name + "* self = ("+ cu_name + "*) calloc(1, sizeof(" + cu_name + "));");
    genCodeLine("    self->free = free;");
    genCodeLine("    " + cu_name + "_ReInit(self, tokenManager);");
    if (Options.getTokenManagerUsesParser())
    	genCodeLine("    self->token_source->setParser(self->token_source, self);");
    genCodeLine("    return self;");
    genCodeLine("}");

    genCodeLine("void delete" + cu_name + "(" + cu_name + "* self)");
    genCodeLine("{");
    genCodeLine("  " + cu_name + "_clear(self);");
    genCodeLine("  self->free(self);");
    genCodeLine("}");
    
    genCodeLine("void " + cu_name + "_ReInit(" + cu_name + "* self, TokenManager* tokenManager)");
    genCodeLine("{");
    genCodeLine("    " + cu_name + "_clear(self);");
    genCodeLine("    self->errorHandler = newErrorHandler();");
    genCodeLine("    self->hasError = false;");
    genCodeLine("    self->token_source = tokenManager;");
    genCodeLine("    self->head = self->token = newToken(0, JJString_FROM_CONST(\"\"));");
    genCodeLine("    self->token->next = NULL;");
    genCodeLine("    self->jj_lookingAhead = false;");
    genCodeLine("    self->jj_rescan = false;");
    genCodeLine("    self->jj_done = false;");
    genCodeLine("    self->jj_scanpos = self->jj_lastpos = NULL;");
    genCodeLine("    self->jj_gc = 0;");
    genCodeLine("    self->jj_kind = -1;");
    genCodeLine("    self->indent = 0;");
    genCodeLine("    self->trace = " + Options.getDebugParser() + ";");
    if (!Options.getStackLimit().equals("")) {
      genCodeLine("    self->jj_stack_limit = "+Options.getStackLimit()+";");
      genCodeLine("    self->jj_stack_error = " + cu_name + "_jj_stack_check(self, true);");
    }

    if (Options.getCacheTokens()) {
      genCodeLine("    self->token->next = self->jj_nt = self->token_source->getNextToken(self->token_source);");
    } else {
      genCodeLine("    self->jj_ntk = -1;");
    }
    if (jjtreeGenerated) {
      genCodeLine("    // jjtree.reset();");
    }
    if (Options.getDepthLimit() > 0) {
      genCodeLine("    self->jj_depth = 0;");
      genCodeLine("    self->jj_depth_error = false;");
    }
    if (Options.getErrorReporting()) {
      genCodeLine("    self->jj_gen = 0;");
      if (maskindex > 0) {
        genCodeLine("    for (int i = 0; i < " + maskindex + "; i++) self->jj_la1[i] = -1;");
      }
    }
    genCodeLine("  }");

    genCodeLine("void " + cu_name + "_clear(" + cu_name + "* self)");
    genCodeLine("{");
    genCodeLine("  if (self->head) {");
    genCodeLine("    Token *next, *t = self->head;");
    genCodeLine("    while (t) {");
    genCodeLine("      next = t->next;");
    genCodeLine("      deleteToken(t);");
    genCodeLine("      t = next;");
    genCodeLine("    }");
    genCodeLine("  }");
    genCodeLine("  if (self->errorHandler) {");
    genCodeLine("    deleteErrorHandler(self->errorHandler); self->errorHandler = NULL;");
    genCodeLine("  }");
    if (Options.getDepthLimit() > 0) {
      genCodeLine("  //assert(self->jj_depth==0);");
    }
    genCodeLine("}");

    switchToIncludeFile();
    genCodeLine("Token * " + cu_name + "_jj_consume_token(" + cu_name + "* self, int kind);");
    switchToMainFile();
    genCodeLine("Token * " + cu_name + "_jj_consume_token(" + cu_name + "* self, int kind)");
    genCodeLine("  {");
    if (!Options.getStackLimit().equals("")) {
      genCodeLine("    if(kind != -1 && (self->jj_stack_error || " + cu_name +"_jj_stack_check(self, false))) {");
      genCodeLine("      if (!self->jj_stack_error) {");
      genCodeLine("        self->errorHandler->handleOtherError(self->errorHandler, JJString_FROM_CONST(\"Stack overflow while trying to parse\"), self);");
      genCodeLine("        self->jj_stack_error=true;");
      genCodeLine("      }");
      genCodeLine("      return " + cu_name + "_jj_consume_token(self, -1);");
      genCodeLine("    }");
    }
    if (Options.getCacheTokens()) {
      genCodeLine("    Token *oldToken = self->token;");
      genCodeLine("    if ((self->token = self->jj_nt)->next != NULL) self->jj_nt = self->jj_nt->next;");
      genCodeLine("    else self->jj_nt = self->jj_nt->next = self->token_source->getNextToken(self->token_source);");
    } else {
      genCodeLine("    Token *oldToken;");
      genCodeLine("    if ((oldToken = self->token)->next != NULL) self->token = self->token->next;");
      genCodeLine("    else self->token = self->token->next = self->token_source->getNextToken(self->token_source);");
      genCodeLine("    self->jj_ntk = -1;");
    }
    genCodeLine("    if (self->token->kind == kind) {");
    if (Options.getErrorReporting()) {
      genCodeLine("      self->jj_gen++;");
      if (jj2index != 0) {
        genCodeLine("      if (++self->jj_gc > 100) {");
        genCodeLine("        self->jj_gc = 0;");
        genCodeLine("        for (int i = 0; i < " + jj2index + "; i++) {");
        genCodeLine("          JJCalls *c = &self->jj_2_rtns[i];");
        genCodeLine("          while (c != NULL) {");
        genCodeLine("            if (c->gen < self->jj_gen) c->first = NULL;");
        genCodeLine("            c = c->next;");
        genCodeLine("          }");
        genCodeLine("        }");
        genCodeLine("      }");
      }
    }
    if (Options.getDebugParser()) {
      genCodeLine("      " + cu_name + "_trace_token(self, self->token, \"\");");
    }
    genCodeLine("      return self->token;");
    genCodeLine("    }");
    if (Options.getCacheTokens()) {
      genCodeLine("    self->jj_nt = self->token;");
    }
    genCodeLine("    self->token = oldToken;");
    if (Options.getErrorReporting()) {
      genCodeLine("    self->jj_kind = kind;");
    }
    if (!Options.getStackLimit().equals("")) {
      genCodeLine("    if (!self->jj_stack_error) {");
    }
    genCodeLine("    JJString image = kind >= 0 ? JJString_FROM_CONST(tokenImage[kind]) : JJString_FROM_CONST(tokenImage[0]);");
    genCodeLine("    self->errorHandler->handleUnexpectedToken(self->errorHandler, kind, image, " + cu_name + "_getToken(self, 1), self);");
    if (!Options.getStackLimit().equals("")) {
      genCodeLine("    }");
    }
    genCodeLine("    self->hasError = true;");
    genCodeLine("    return self->token;");
    genCodeLine("  }");

    if (jj2index != 0) {
      switchToIncludeFile();
      genCodeLine("bool " + cu_name + "_jj_scan_token(" + cu_name + "* self, int kind);");
      switchToMainFile();
      genCodeLine("bool " + cu_name + "_jj_scan_token(" + cu_name + "* self, int kind)");
      genCodeLine("{");
      if (!Options.getStackLimit().equals("")) {
        genCodeLine("    if(kind != -1 && (self->jj_stack_error || " + cu_name + "_jj_stack_check(self, false))) {");
        genCodeLine("      if (!self->jj_stack_error) {");
        genCodeLine("        self->errorHandler->handleOtherError(self->errorHandler, JJString_FROM_CONST(\"Stack overflow while trying to parse\"), self);");
        genCodeLine("        self->jj_stack_error=true;");
        genCodeLine("      }");
        genCodeLine("      return " + cu_name + "_jj_consume_token(self, -1);");
        genCodeLine("    }");
      }
      genCodeLine("    if (self->jj_scanpos == self->jj_lastpos) {");
      genCodeLine("      self->jj_la--;");
      genCodeLine("      if (self->jj_scanpos->next == NULL) {");
      genCodeLine("        self->jj_lastpos = self->jj_scanpos = self->jj_scanpos->next = self->token_source->getNextToken(self->token_source);");
      genCodeLine("      } else {");
      genCodeLine("        self->jj_lastpos = self->jj_scanpos = self->jj_scanpos->next;");
      genCodeLine("      }");
      genCodeLine("    } else {");
      genCodeLine("      self->jj_scanpos = self->jj_scanpos->next;");
      genCodeLine("    }");
      if (Options.getErrorReporting()) {
        genCodeLine("    if (self->jj_rescan) {");
        genCodeLine("      int i = 0; Token *tok = self->token;");
        genCodeLine("      while (tok != NULL && tok != self->jj_scanpos) { i++; tok = tok->next; }");
        genCodeLine("      if (tok != NULL) " + cu_name + "_jj_add_error_token(self, kind, i);");
        if (Options.getDebugLookahead()) {
          genCodeLine("    } else {");
          genCodeLine("      " + cu_name + "_trace_scan(self, self->jj_scanpos, kind);");
        }
        genCodeLine("    }");
      } else if (Options.getDebugLookahead()) {
        genCodeLine("    " + cu_name + "_trace_scan(self, self->jj_scanpos, kind);");
      }
      genCodeLine("    if (self->jj_scanpos->kind != kind) return true;");
      genCodeLine("    if (self->jj_la == 0 && self->jj_scanpos == self->jj_lastpos) { return self->jj_done = true; }");
      genCodeLine("    return false;");
      genCodeLine("  }");
    }
    
    switchToIncludeFile();
    genCodeLine("Token * " + cu_name + "_getNextToken(" + cu_name + "* self);");
    switchToMainFile();
    genCodeLine("Token * " + cu_name + "_getNextToken(" + cu_name + "* self)");
    genCodeLine("{");
    if (Options.getCacheTokens()) {
      genCodeLine("    if ((self->token = self->jj_nt)->next != NULL) self->jj_nt = self->jj_nt->next;");
      genCodeLine("    else self->jj_nt = self->jj_nt->next = self->token_source->getNextToken(self->token_source);");
    } else {
      genCodeLine("    if (self->token->next != NULL) self->token = self->token->next;");
      genCodeLine("    else self->token = self->token->next = self->token_source->getNextToken(self->token_source);");
      genCodeLine("    self->jj_ntk = -1;");
    }
    if (Options.getErrorReporting()) {
      genCodeLine("    self->jj_gen++;");
    }
    if (Options.getDebugParser()) {
      genCodeLine("      " + cu_name + "_trace_token(self, self->token, \" (in getNextToken)\");");
    }
    genCodeLine("    return self->token;");
    genCodeLine("  }");

    switchToIncludeFile();
    genCodeLine("Token * " + cu_name + "_getToken(" + cu_name + "* self, int index);");
    switchToMainFile();
    genCodeLine("Token * " + cu_name + "_getToken(" + cu_name + "* self, int index)");
    genCodeLine("{");
    if (lookaheadNeeded) {
      genCodeLine("    Token *t = self->jj_lookingAhead ? self->jj_scanpos : self->token;");
    } else {
      genCodeLine("    Token *t = self->token;");
    }
    genCodeLine("    for (int i = 0; i < index; i++) {");
    genCodeLine("      if (t->next != NULL) t = t->next;");
    genCodeLine("      else t = t->next = self->token_source->getNextToken(self->token_source);");
    genCodeLine("    }");
    genCodeLine("    return t;");
    genCodeLine("  }");

    if (!Options.getCacheTokens()) {
      switchToIncludeFile();
      genCodeLine("int " + cu_name + "_jj_ntk_f(" + cu_name + "* self);");
      switchToMainFile();
      genCodeLine("int " + cu_name + "_jj_ntk_f(" + cu_name + "* self)");
      genCodeLine("{");

      genCodeLine("    if ((self->jj_nt=self->token->next) == NULL)");
      genCodeLine("      return (self->jj_ntk = (self->token->next=self->token_source->getNextToken(self->token_source))->kind);");
      genCodeLine("    else");
      genCodeLine("      return (self->jj_ntk = self->jj_nt->kind);");
      genCodeLine("  }");
    }

    if (Options.getErrorReporting()) {
      if (jj2index != 0) {
        switchToIncludeFile();
        genCodeLine("void " + cu_name + "_jj_add_error_token(" + cu_name + "* self, int kind, int pos);");
        switchToMainFile();
        genCodeLine("void " + cu_name + "_jj_add_error_token(" + cu_name + "* self, int kind, int pos)");
        genCodeLine("  {");
        genCodeLine("  }");
      }

      switchToIncludeFile();
      genCodeLine("void " + cu_name + "_parseError(" + cu_name + "* self);");
      switchToMainFile();
      genCodeLine("void " + cu_name + "_parseError(" + cu_name + "* self)");
      genCodeLine("   {");
      genCodeLine("      fprintf(stderr, \"Parse error at: %d:%d, after token: %s encountered: %s\\n\", self->token->beginLine, self->token->beginColumn, self->token->image.str, " + cu_name + "_getToken(self, 1)->image.str);");
      genCodeLine("   }");
    } else {
      switchToIncludeFile();
      genCodeLine("void " + cu_name + "_parseError(" + cu_name + "* self);");
      switchToMainFile();
      genCodeLine("void " + cu_name + "_parseError(" + cu_name + "* self)");
      genCodeLine("   {");
      genCodeLine("      fprintf(stderr, \"Parse error at: %d:%d, after token: %s encountered: %s\\n\", self->token->beginLine, self->token->beginColumn, self->token->image.str, " + cu_name + "_getToken(self, 1)->image.str);");
      genCodeLine("   }");
    }

    switchToIncludeFile();
    genCodeLine("bool " + cu_name + "_trace_enabled(" + cu_name + "* self);");
    genCodeLine("void " + cu_name + "_enable_tracing(" + cu_name + "* self);");
    genCodeLine("void " + cu_name + "_disable_tracing(" + cu_name + "* self);");
    
    switchToMainFile();
    genCodeLine("bool " + cu_name + "_trace_enabled(" + cu_name + "* self)");
    genCodeLine("  {");
    genCodeLine("    return self->trace;");
    genCodeLine("  }");
    
   if (Options.getDebugParser()) {
      switchToIncludeFile();
      genCodeLine("void " + cu_name + "_trace_call(" + cu_name + "* self, const char *s);");
      genCodeLine("void " + cu_name + "_trace_return(" + cu_name + "* self, const char *s);");
      genCodeLine("void " + cu_name + "_trace_token(" + cu_name + "* self, Token *t, const char *where);");
      genCodeLine("void " + cu_name + "_trace_scan(" + cu_name + "* self, Token *t1, int t2);");
      
      switchToMainFile();
      genCodeLine("void " + cu_name + "_enable_tracing(" + cu_name + "* self)");
      genCodeLine("{");
      genCodeLine("    self->trace = true;");
      genCodeLine("}");

      genCodeLine("void " + cu_name + "_disable_tracing(" + cu_name + "* self)");
      genCodeLine("{");
      genCodeLine("    self->trace = false;");
      genCodeLine("}");

      genCodeLine("void " + cu_name + "_trace_call(" + cu_name + "* self, const char *s)");
      genCodeLine("  {");
      genCodeLine("    if (" + cu_name + "_trace_enabled(self)) {");
      genCodeLine("      for (int i = 0; i < self->indent; i++) { printf(\" \"); }");
      genCodeLine("      printf(\"Call:   %s\\n\", s);");
      genCodeLine("    }");
      genCodeLine("    self->indent = self->indent + 2;");
      genCodeLine("  }");

      genCodeLine("void " + cu_name + "_trace_return(" + cu_name + "* self, const char *s)");
      genCodeLine("  {");
      genCodeLine("    self->indent = self->indent - 2;");
      genCodeLine("    if (" + cu_name + "_trace_enabled(self)) {");
      genCodeLine("      for (int i = 0; i < self->indent; i++) { printf(\" \"); }");
      genCodeLine("      printf(\"Return: %s\\n\", s);");
      genCodeLine("    }");
      genCodeLine("  }");

      genCodeLine("void " + cu_name + "_trace_token(" + cu_name + "* self, Token *t, const char *where)");
      genCodeLine("  {");
      genCodeLine("    if (" + cu_name + "_trace_enabled(self)) {");
      genCodeLine("      for (int i = 0; i < self->indent; i++) { printf(\" \"); }");
      genCodeLine("      printf(\"Consumed token: <kind: %d(%s), \\\"%s\\\"\", t->kind, tokenImage[t->kind], t->image.str);");
      genCodeLine("      printf(\" at line %d column %d> %s\\n\", t->beginLine, t->beginColumn, where);");
      genCodeLine("    }");
      genCodeLine("  }");

      genCodeLine("void " + cu_name + "_trace_scan(" + cu_name + "* self, Token *t1, int t2)");
      genCodeLine("  {");
      genCodeLine("    if (" + cu_name + "_trace_enabled(self)) {");
      genCodeLine("      for (int i = 0; i < self->indent; i++) { printf(\" \"); }");
      genCodeLine("      printf(\"Visited token: <Kind: %d(%s), \\\"%s\\\"\", t1->kind, tokenImage[t1->kind], t1->image.str);");
      genCodeLine("      printf(\" at line %d column %d>; Expected token: %s\\n\", t1->beginLine, t1->beginColumn, tokenImage[t2]);");
      genCodeLine("    }");
      genCodeLine("  }");
    } else {
      switchToMainFile();
      genCodeLine("void " + cu_name + "_enable_tracing(" + cu_name + "* self)");
      genCodeLine("  {");
      genCodeLine("  }");
      genCodeLine("void " + cu_name + "_disable_tracing(" + cu_name + "* self)");
      genCodeLine("  {");
      genCodeLine("  }");
    }

    if (jj2index != 0 && Options.getErrorReporting()) {
      switchToIncludeFile();
      genCodeLine("void " + cu_name + "_jj_rescan_token(" + cu_name + "* self);");
      genCodeLine("void " + cu_name + "_jj_save(" + cu_name + "* self, int index, int xla);");
      switchToMainFile();
      genCodeLine("void " + cu_name + "_jj_rescan_token(" + cu_name + "* self)");
      genCodeLine("{");
      genCodeLine("    self->jj_rescan = true;");
      genCodeLine("    for (int i = 0; i < " + jj2index + "; i++) {");
      genCodeLine("      JJCalls *p = &self->jj_2_rtns[i];");
      genCodeLine("      do {");
      genCodeLine("        if (p->gen > self->jj_gen) {");
      genCodeLine("          self->jj_la = p->arg; self->jj_lastpos = self->jj_scanpos = p->first;");
      genCodeLine("          switch (i) {");
      for (int i = 0; i < jj2index; i++) {
        genCodeLine("            case " + i + ": jj_3_" + (i+1) + "(); break;");
      }
      genCodeLine("          }");
      genCodeLine("        }");
      genCodeLine("        p = p->next;");
      genCodeLine("      } while (p != NULL);");
      genCodeLine("    }");
      genCodeLine("    self->jj_rescan = false;");
      genCodeLine("  }");

      genCodeLine("void " + cu_name + "_jj_save(" + cu_name + "* self, int index, int xla)");
      genCodeLine("{");
      genCodeLine("    JJCalls *p = &self->jj_2_rtns[index];");
      genCodeLine("    while (p->gen > self->jj_gen) {");
      genCodeLine("      if (p->next == NULL) { p = p->next = (JJCalls*)calloc(1, sizeof(JJCalls)); break; }");
      genCodeLine("      p = p->next;");
      genCodeLine("    }");
      genCodeLine("    p->gen = self->jj_gen + xla - self->jj_la; p->first = self->token; p->arg = xla;");
      genCodeLine("  }");
    }

    if (cu_from_insertion_point_2.size() != 0) {
      printTokenSetup((cu_from_insertion_point_2.get(0))); this.ccol = 1;
      for (Iterator<?> it = cu_from_insertion_point_2.iterator(); it.hasNext();) {
        t = (Token)it.next();
        printToken(t);
      }
      printTrailingComments(t);
    }
    genCodeLine("");

    switchToIncludeFile();

    Token t1 = JavaCCGlobals.otherLanguageDeclTokenBeg;
    Token t2 = JavaCCGlobals.otherLanguageDeclTokenEnd;
    while(t1 != t2) {
      printToken(t1);
      t1 = t1.next;
    }
    genCodeLine("\n");

    saveOutput(Options.getOutputDirectory() + File.separator + cu_name + getFileExtension(Options.getOutputLanguage()));
  }

   public static void reInit()
   {
      lookaheadNeeded = false;
   }

}
