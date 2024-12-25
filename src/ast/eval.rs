//! todo

use crate::continuation::*;
use super::*;

impl Body {
    pub async fn tail_eval(
        &self,
        env: &Env,
        cont: &Option<Arc<Continuation>>,
    ) -> Result<ValuesOrPreparedCall, RuntimeError> {
        let Some(last) = self.forms.last() else {
            return Ok(ValuesOrPreparedCall::Values(vec![Gc::new(Value::Null)]));
        };
        for (form, tail) in self.forms.skip_last() {
            let cont = Some(Arc::new(Continuation::new(
                Arc::new(ResumableBody::new(env, &tail)),
                cont,
            )));
            // Discard values that aren't returned
            form.eval(env, &cont).await?;
        }
        last.tail_eval(env, cont).await
    }
}
