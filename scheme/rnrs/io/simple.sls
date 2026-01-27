(library (rnrs io simple (6))
  (export eof-object eof-object? input-port? output-port? close-input-port
          close-output-port close-port (import (rnrs io simple builtins)))
  (import (rnrs base)
          (only (rnrs io ports)
                eof-object eof-object? input-port? output-port? close-port))

  ;; TODO: Should we raise an error if the port is the wrong type? It's kind of
  ;; a mystery why these functions should be differentiated at all

  (define (close-input-port port)
    (close-port port))

  (define (close-output-port port)
    (close-port port)))
                        
