# The Scanner class

class Scanner(self):
    def __init__(self, source):
        this.source = source
        this.tokens = []
        this.start = 0
        this.current = 0
        this.line = 1
        
        # this.keywords = Set(
        #     "" 
        # )
    
    def is_at_end(self):
        return this.current >= len(source)
    
    def scan_token(self):
        char = advance()
        # Switch statement for 
    
    def scan_tokens(self):
        while (not isAtEnd()):
            # We are at the start of the next token
            start = current
            scan_token()
            
        this.tokens.append(Token("EOF", "", null, line))
        return this.tokens