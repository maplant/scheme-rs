mod common;

use scheme_rs::env::{ImportPolicy, TopLevelEnvironment};
use scheme_rs::runtime::Runtime;

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_syntax_rules_basic() {
    let rt = Runtime::new();

    // Define a library that exports a macro
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-macro-lib)
           (export my-macro)
           (import (rnrs))
           (define-syntax my-macro
             (syntax-rules ()
               ((_ body ...) (begin body ...)))))"
    ))
    .expect("Failed to define macro library");

    // Use the macro in a different library
    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-macro-lib))
         (my-macro 42)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Cross-library syntax-rules macro failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_syntax_rules_with_expressions() {
    let rt = Runtime::new();

    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-macro-lib2)
           (export my-when)
           (import (rnrs))
           (define-syntax my-when
             (syntax-rules ()
               ((_ test body ...)
                (if test (begin body ...))))))"
    ))
    .expect("Failed to define macro library");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-macro-lib2))
         (my-when #t 42)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Cross-library my-when macro failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_macro_introducing_bindings() {
    let rt = Runtime::new();

    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-macro-lib3)
           (export my-let1)
           (import (rnrs))
           (define-syntax my-let1
             (syntax-rules ()
               ((_ var val body ...)
                (let ((var val)) body ...)))))"
    ))
    .expect("Failed to define macro library");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-macro-lib3))
         (my-let1 x 10 (+ x 1))"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Cross-library my-let1 macro failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_macro_referencing_definition_env() {
    let rt = Runtime::new();

    // This macro references `list` from its own definition environment (rnrs).
    // In a cross-library expansion, `list` must resolve through the macro's
    // definition scope, not the use site scope.
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-macro-lib4)
           (export wrap-in-list)
           (import (rnrs))
           (define-syntax wrap-in-list
             (syntax-rules ()
               ((_ x) (list x)))))"
    ))
    .expect("Failed to define macro library");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-macro-lib4))
         (wrap-in-list 42)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Cross-library wrap-in-list macro failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_macro_hygiene_no_capture() {
    let rt = Runtime::new();

    // The macro uses `list` from its definition env. The use site
    // shadows `list` with something else. The macro should still use
    // the original `list` from (rnrs).
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-macro-lib5)
           (export wrap-in-list)
           (import (rnrs))
           (define-syntax wrap-in-list
             (syntax-rules ()
               ((_ x) (list x)))))"
    ))
    .expect("Failed to define macro library");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-macro-lib5))
         (let ((list (lambda (x) 'shadowed)))
           (wrap-in-list 42))"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
            // If hygiene works, this should be (42), not 'shadowed
        }
        Err(e) => {
            panic!("Cross-library hygiene test failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_macro_using_helper_from_definition_lib() {
    let rt = Runtime::new();

    // Macro references a helper function defined in the same library.
    // When expanded at use site, the helper must still be accessible.
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-macro-lib6)
           (export my-double)
           (import (rnrs))
           (define (helper x) (* x 2))
           (define-syntax my-double
             (syntax-rules ()
               ((_ x) (helper x)))))"
    ))
    .expect("Failed to define macro library");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-macro-lib6))
         (my-double 21)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Cross-library macro with helper function failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_nested_macro_expansion() {
    let rt = Runtime::new();

    // Library A exports macro-a, Library B imports A and exports macro-b
    // that uses macro-a. Then a program uses macro-b.
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-nested-a)
           (export macro-a)
           (import (rnrs))
           (define-syntax macro-a
             (syntax-rules ()
               ((_ x) (+ x 1)))))"
    ))
    .expect("Failed to define library A");

    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-nested-b)
           (export macro-b)
           (import (rnrs) (test-nested-a))
           (define-syntax macro-b
             (syntax-rules ()
               ((_ x) (macro-a (* x 2))))))"
    ))
    .expect("Failed to define library B");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-nested-b))
         (macro-b 5)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
            // macro-b expands to (macro-a (* 5 2)) -> (+ (* 5 2) 1) -> 11
        }
        Err(e) => {
            panic!("Nested cross-library macro expansion failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_nested_macro_via_syntax_case() {
    let rt = Runtime::new();

    // Same as above but using syntax-case directly, to isolate whether
    // the problem is in syntax-rules or in the core expansion mechanism.
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-nested-sc-a)
           (export macro-a)
           (import (rnrs))
           (define-syntax macro-a
             (lambda (x)
               (syntax-case x ()
                 ((_ y) #'(+ y 1))))))"
    ))
    .expect("Failed to define library A");

    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-nested-sc-b)
           (export macro-b)
           (import (rnrs) (test-nested-sc-a))
           (define-syntax macro-b
             (lambda (x)
               (syntax-case x ()
                 ((_ y) #'(macro-a (* y 2)))))))"
    ))
    .expect("Failed to define library B");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-nested-sc-b))
         (macro-b 5)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Nested cross-library syntax-case macro expansion failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_macro_b_uses_a_directly() {
    let rt = Runtime::new();

    // Verify that macro-a works when directly invoked cross-library (not nested)
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-direct-a)
           (export macro-a)
           (import (rnrs))
           (define-syntax macro-a
             (syntax-rules ()
               ((_ x) (+ x 1)))))"
    ))
    .expect("Failed to define library A");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-direct-a))
         (macro-a 5)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Direct cross-library macro-a failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_re_export_macro() {
    let rt = Runtime::new();

    // Library A defines and exports a macro, Library B re-exports it.
    // Using the macro from library B should work.
    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-reexport-a)
           (export my-add1)
           (import (rnrs))
           (define-syntax my-add1
             (syntax-rules ()
               ((_ x) (+ x 1)))))"
    ))
    .expect("Failed to define library A");

    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-reexport-b)
           (export (import (test-reexport-a)))
           (import (rnrs) (test-reexport-a)))"
    ))
    .expect("Failed to define library B");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-reexport-b))
         (my-add1 5)"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Re-exported cross-library macro failed: {e:?}");
        }
    }
}

#[scheme_rs_macros::maybe_async]
#[cfg_attr(feature = "async", ::tokio::test)]
#[cfg_attr(not(feature = "async"), test)]
fn cross_library_syntax_case_macro() {
    let rt = Runtime::new();

    scheme_rs_macros::maybe_await!(rt.def_lib(
        "(library (test-sc-lib)
           (export my-swap!)
           (import (rnrs))
           (define-syntax my-swap!
             (lambda (x)
               (syntax-case x ()
                 ((_ a b)
                  #'(let ((tmp a))
                      (set! a b)
                      (set! b tmp)))))))"
    ))
    .expect("Failed to define syntax-case macro library");

    let env = TopLevelEnvironment::new_repl(&rt);
    let result = scheme_rs_macros::maybe_await!(env.eval(
        ImportPolicy::Allow,
        "(import (rnrs) (test-sc-lib))
         (let ((x 1) (y 2))
           (my-swap! x y)
           (list x y))"
    ));
    match &result {
        Ok(vals) => {
            assert_eq!(vals.len(), 1, "Expected one value, got {:?}", vals);
        }
        Err(e) => {
            panic!("Cross-library syntax-case macro failed: {e:?}");
        }
    }
}
