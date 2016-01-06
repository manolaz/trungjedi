Clone this wiki locally
Introduction

=========================================================

TERMINAL PARTIES

$ sudo apt-get install libsdl* 


$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ curl -O https://beta.quicklisp.org/quicklisp.lisp.asc

$ sbcl
=========================================================
  ==== quicklisp quickstart loaded ====

* (quicklisp-quickstart:install)

* (ql:quickload "vecto")

* (ql:add-to-init-file)

* (quit)
* (quicklisp:quickload :cl-opengl)
* (quicklisp:quickload :lispbuilder-sdl)
* (ql:quickload "lispbuilder-sdl-gfx")



The CLinch graphics engine is meant to be a simpler alternative for 2 and 3 dimensional graphics development in Lisp. It is meant to only be a graphics engine so it doesn't contain any windowing libraries or game asset importers, etc. This is by design, in order to make CLinch more useful to a wide array of developers. It currently requires OpenGL (through CL-OpenGL). It also includes binders for the Cairo Vector Graphics Library, Pango Test Layout Engine, and FreeImage File Format Read/Write Library.

Finally, go to your "~/quicklisp/local-projects/" directory (this will be different in windows) and type:

git clone http://github.com/BradWBeer/CLinch.git

This should create a directory named "CLinch". Quicklisp looks in "local-projects" automatically, so this will let you 'quickload' CLinch the same as the other packages. If you don't have git, you can find it here. In you slime session load CLinch with the following to verify everything works:

* (quicklisp:quickload :clinch)

* (ql:quickload :lispbuilder-sdl)
* (ql:quickload :clinch)


Save, load the file and run (main). You should see a black window with the title "Tutorial 1". Hopefully it was fairly easy.
