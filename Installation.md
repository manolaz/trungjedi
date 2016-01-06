Clone this wiki locally
Introduction

The CLinch graphics engine is meant to be a simpler alternative for 2 and 3 dimensional graphics development in Lisp. It is meant to only be a graphics engine so it doesn't contain any windowing libraries or game asset importers, etc. This is by design, in order to make CLinch more useful to a wide array of developers. It currently requires OpenGL (through CL-OpenGL). It also includes binders for the Cairo Vector Graphics Library, Pango Test Layout Engine, and FreeImage File Format Read/Write Library.

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

$ 

For these tutorials, you will need a implementation of Common Lisp. I recommend SBCL, as that is what I use. You can find it here. I will also be using QuickLisp, a wonderful package management tool. Follow these instructions, if you don't already have it. Next you will need CL-OpenGL and Lispbuilder-SDL. To install these, start up an REPL and use quicklisp:quickload to install them as follows:

* (quicklisp:quickload :cl-opengl)
* (quicklisp:quickload :lispbuilder-sdl)
* (ql:quickload "lispbuilder-sdl-gfx")

Finally, go to your "~/quicklisp/local-projects/" directory (this will be different in windows) and type:

git clone http://github.com/BradWBeer/CLinch.git

This should create a directory named "CLinch". Quicklisp looks in "local-projects" automatically, so this will let you 'quickload' CLinch the same as the other packages. If you don't have git, you can find it here. In you slime session load CLinch with the following to verify everything works:

* (quicklisp:quickload :clinch)

Congratulations, you're system should be setup properly for CLinch.
Creating a Window

In order to creating a window, we need to load up our windowing library Lispbuilder-SDL and CLinch. So open up a new file (I named my "sdl-tutorial01.lisp") and add the following lines:

* (ql:quickload :lispbuilder-sdl)
* (ql:quickload :clinch)


Save, load the file and run (main). You should see a black window with the title "Tutorial 1". Hopefully it was fairly easy.
