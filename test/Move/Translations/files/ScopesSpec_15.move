module NamedAddr::TestingModule {

    // CPS
    fun my_fun(r: &mut u64): u64 {
        *r = 6;

        12
    }

    // CPS
    fun my_unit(r: &mut u64) {
        *r = 6;
    }

    // CPS
    fun test(a: u64) {
        // PUSH scope
        // POST b
        let b = 12;
        let r = &mut b;

        // The else branch, if omitted, defaults to (), so the if must return the same
        // let (temp_i, scopes) = if (a > 0) {
        //      PUSH scope
        //      let (temp_i, scopes) = my_unit(r, scopes);
        //      (temp_i, tail scopes)
        //} else {
        //      (_, scopes)
        //}
        if (a > 0) my_unit(r);
        // Also, temp_i; as its own seq item

        // Unaltered
        let d = if (a > 0) 1 else 2;

        // let (_, scopes) = if (a > 0) {
        //      PUSH scope
        //      // Note how the end expression is a sequence itself
        //      ({b}, tail scopes)
        //} else {
        //      PUSH scope
        //      let (temp_i, scopes) = {
        //          PUSH scope
        //          let (temp_i, scopes) = my_fun(r, scopes);
        //          (temp_i, tail scopes)
        //      }
        //      (temp_i, tail scopes)
        //}
        let e = if (a > 0) { b }
        else {
            my_fun(r)
        };
        // let e = temp_i;
        // (_, tail scopes)
    }
}
