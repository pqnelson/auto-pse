# Auto-Pse (pronounced "Autopsy")

A problem solving environment (PSE) for physics problems. Originally
inspired by SCMUTILS and ISAAC, this combines aspects of computer
algebra systems with GOFAI.

## Conventions

The basic conventions I've chosen to follow is to overload Common Lisp
functions and, when I have to add new things (like integrating
functions), to use names and parameters from Mathematica. 

For example, matrix exponentation would be something like:

```lisp
(defmethod exp ((x matrix))
  ;; ...
  )
```

Uh, I should also probably double check I can overload built-in
operations like this...


