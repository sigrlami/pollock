<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="">
    <meta name="author" content="">

    <title>Pollock - best e-voting system</title>
    <link href="static/css/bootstrap.min.css" rel="stylesheet">
    <link href="static/css/app.css" rel="stylesheet">
    <link href="https://fonts.googleapis.com/css?family=Montserrat" rel="stylesheet">    
</head>

<!-- The #page-top ID is part of the scrolling feature - the data-spy and data-target are part of the built-in Bootstrap scrollspy function -->

<body id="page-top" data-spy="scroll" data-target=".navbar-fixed-top">

    <!-- Navigation -->
    <nav class="navbar navbar-default navbar-fixed-top" role="navigation">
        <div class="container">
            <div class="navbar-header page-scroll">
                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-ex1-collapse">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <a class="navbar-brand page-scroll" href="/">
                   <img style="height: 30px; margin-top: -5px;" src="static/img/logo.png">
		</a>
		<ifLoggedOut> <a class="navbar-brand brand page-scroll" href="/"> pollock </a> </ifLoggedOut>
		<ifLoggedIn> <a class="navbar-brand brand page-scroll" href="/app"> pollock </a> </ifLoggedIn>
            </div>
            <!-- Collect the nav links, forms, and other content for toggling -->
            <div class="collapse navbar-collapse navbar-ex1-collapse">
                <ul class="nav navbar-nav pull-right">
                    <!-- Hidden li included to remove active class from about link when scrolled up past about section -->
                    <li class="hidden">
                        <a class="page-scroll" href="#page-top"></a>
                    </li>
		    <li>
                      <ifLoggedIn>
			 <a href="/channels" class="btn btn-menu"><span class="brand glyphicon glyphicon-align-left"></span></a>
                      </ifLoggedIn> 
                    </li>
                    <li>
                      <ifLoggedIn>
			 <a href="/polls/new" class="btn btn-menu"><span class="brand glyphicon glyphicon-plus"></span></a>
                  </ifLoggedIn> 
                    </li>
                    <li>
		     <ifLoggedOut> 
                     <div class="btn-group" role="group" aria-label="..." style="margin-top: 7px;">
                       <a class="btn btn-nb-white-bold" href="/signup" role="button">Sign Up</a>
		       <a class="btn btn-nb-white" href="/login" role="button">Log In</a>
                     </div>
		     </ifLoggedOut>
		     <ifLoggedIn>
		       <div class="btn-group" role="group" aria-label="..." style="margin-top: 7px;">
		       <a class="btn btn-nb-white-bold" href="#"><loggedInUser/></a>
		       <a class="btn btn-nb-white" href="/logout" role="button">Log Out</a>
                     </div>
		     </ifLoggedIn>  
                    </li>
                </ul>
            </div>
            <!-- /.navbar-collapse -->
        </div>
        <!-- /.container -->
    </nav>

    <!-- Intro Section -->
    <section id="intro" class="intro-section-main">
        <div class="container header">
	  <div class="text-vertical-center">
	    <h1>Modern E-Voting</h1>
	    <h3>Power your polls with zero-knowledge proof of authenticity</h3>
	  </div>  
        </div>
    </section>

    <footer class="navbar navbar-bottom footer">
      <div class="copyright">
	<h4 id="left">&copy; 2017, </h4> <h4 id="right"> Kelecorix, Inc </h4>
      </div>	
    </footer>
<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-90196008-1', 'auto');
  ga('send', 'pageview');

</script>
    <script src="static/js/jquery.js"></script>
    <script src="static/js/bootstrap.min.js"></script>
    <script src="static/js/jquery.easing.min.js"></script>
    <script src="static/js/scrolling-nav.js"></script>
</body>
</html>
