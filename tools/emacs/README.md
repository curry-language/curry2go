This directory contains a Curry mode for Emacs adapted from Haskell.
To use it, add the following to your `.emacs` file (adapt the first
line according to your local Curry2Go installation):

    (setq load-path (cons "/home/joe/Curry2Go/tools/emacs/" load-path))
    (setq auto-mode-alist
          (append auto-mode-alist
                  '(("\\.curry$"  . curry-mode)
                    ("\\.lcurry$"  . literate-curry-mode))))
    (autoload 'curry-mode "curry-mode"
             "Major mode for editing Curry programs." t)
    (autoload 'literate-curry-mode "curry-mode"
             "Major mode for editing literate Curry scripts." t)
    
    (add-hook 'curry-mode-hook 'turn-on-curry-font-lock)
    (add-hook 'curry-mode-hook 'turn-on-curry-decl-scan)
    (add-hook 'curry-mode-hook 'turn-on-curry-pakcs)

If you have "curry2go" in your load path, then you can load Curry programs
from a buffer containing a Curry program with the following keys:

* `C-c C-l`: load file into Curry2Go
* `C-c C-r`: reload file into Curry2Go
* `C-c C-b`: go to Curry2Go buffer
