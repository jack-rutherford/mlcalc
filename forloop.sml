fun forloop (0, f, x) = 0
       | forloop (y, f, x) = (f x; forloop(y-1, f, x));
	   
forloop(10, TextIO.output, (TextIO.stdOut, "hello\n"))