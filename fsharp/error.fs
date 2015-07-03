module Error

    exception ReaderError of string
    exception EvalError of string
    exception MalError of Types.Node
    
    let expectedXButEOF x = ReaderError(sprintf "Expected %s, got EOF" x)
    let expectedX x = ReaderError(sprintf "Expected %s" x)
    let unexpectedChar () = ReaderError("Unexpected char")
    let invalidToken () = ReaderError("Invalid token")
    
    let expectedEvenNodeCount () = EvalError("Expected even node count")
    let wrongArity () = EvalError("Arity: wrong number of arguments")
    let argMismatch () = EvalError("Argument mismatch")
    let symbolNotFound s = EvalError(sprintf "'%s' not found" s)
    let noEnvironment () = EvalError("No environment")
    let tooManyValues () = EvalError("Too many values")
    let notEnoughValues () = EvalError("Not enough values")
    let onlyOneSymbolAfterAmp () = EvalError("only one symbol after &")
    let errExpectedX x = EvalError(sprintf "expected %s" x)
    let indexOutOfBounds () = EvalError("Index out of bounds")
