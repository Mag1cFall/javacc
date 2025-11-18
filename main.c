#include <stdio.h>
#include <stdlib.h>
#include "CParser.h"
#include "CharStream.h"
#include "CParserTokenManager.h"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Usage: %s <input>\n", argv[0]);
        return 1;
    }

    CharStream* stream = (CharStream*)new_SimpleCharStream_from_string(argv[1]);
    if (!stream) {
        printf("Failed to create CharStream\n");
        return 1;
    }

    CParserTokenManager* tokenManager = newCParserTokenManager(stream);
    if (!tokenManager) {
        printf("Failed to create TokenManager\n");
        return 1;
    }

    CParser* parser = newCParser((TokenManager*)tokenManager);
    if (!parser) {
        printf("Failed to create Parser\n");
        return 1;
    }

    parse(parser);
    
    printf("Parsing completed successfully.\n");
    
    return 0;
}
