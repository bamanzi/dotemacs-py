@rem -u  unbuffered binary stdout and stderr; also PYTHONUNBUFFERED=x
@rem http://pswinkels.blogspot.com/2010/04/debugging-python-code-from-within-emacs.html
@rem http://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Subprocess-buffering.html#Subprocess-buffering
@python3 -u -mpdbpp %*
