<apply template="base">
  <bind tag="pagetitle">Log in</bind>
  <div class="row" style="margin-top:60px;">
    <div class="col-md-4 col-md-offset-4">
      <form method="POST" action="/signup" accept-charset="UTF-8" role="form" id="loginform" class="form-signin">
        <fieldset>
          <h3 class="sign-up-title" style="color:dimgray; text-align: center">Welcome!</h3>
          <hr class="colorgraph">
          <input class="form-control email-title" placeholder="Username" name="username" id="username" type="text">
          <input class="form-control" placeholder="Password" name="password" type="password" id="password" value="">
          <input class="btn btn-lg btn-success btn-block" type="submit" value="Sign Up">
          <br>
          <p class="text-center"><a href="/login">Already have an account?</a></p>
        </fieldset>
      </form>
    </div>
  </div>
</apply>