#include <stdio.h>
#include <stdlib.h>
#include "CParser.h"
#include "CharStream.h"
#include "CParserTokenManager.h"

int main(int argc, char* argv[]) {
    printf("DEBUG: Program started\n");
    fflush(stdout);
    
    if (argc < 2) {
        printf("Usage: %s <input>\n", argv[0]);
        return 1;
    }

    printf("DEBUG: Creating CharStream from input: %s\n", argv[1]);
    fflush(stdout);
    
    CharStream* stream = (CharStream*)new_SimpleCharStream_from_string(argv[1]);
    if (!stream) {
        printf("Failed to create CharStream\n");
        return 1;
    }

    printf("DEBUG: CharStream created successfully\n");
    fflush(stdout);

    CParserTokenManager* tokenManager = newCParserTokenManager(stream);
    if (!tokenManager) {
        printf("Failed to create TokenManager\n");
        return 1;
    }

    printf("DEBUG: TokenManager created successfully\n");
    fflush(stdout);

    CParser* parser = newCParser((TokenManager*)tokenManager);
    if (!parser) {
        printf("Failed to create Parser\n");
        return 1;
    }

    printf("DEBUG: Parser created, calling parse()\n");
    fflush(stdout);

    parse(parser);
    
    printf("DEBUG: parse() returned, hasError=%d\n", parser->hasError);
    fflush(stdout);
    
    if (parser->hasError) {
        printf("Parse failed with error.\n");
        return 1;
    }
    
    printf("Parsing completed successfully.\n");
    
    return 0;
}
