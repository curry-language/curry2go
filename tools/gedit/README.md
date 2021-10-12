To locally install syntax highlighting support for Curry, you can use one
of the following two commands (depending on your actual GTK version):

    cp *.lang ~/.local/share/gtksourceview-2.0/language-specs
    cp *.lang ~/.local/share/gtksourceview-3.0/language-specs

You may have to create the corresponding directory which can be done
with one of the following commands:

    mkdir -p ~/.local/share/gtksourceview-2.0/language-specs
    mkdir -p ~/.local/share/gtksourceview-3.0/language-specs

If you wish to install the syntax highlighting files globally, you can
instead use one of the following commands (again depending on your GTK
version):

    cp *.lang /usr/share/gtksourceview-2.0/language-specs
    cp *.lang /usr/share/gtksourceview-3.0/language-specs

Note, that you are required to have appropriate permissions to copy files
into the specified folders.

In both cases, you have to restart gedit for the installation to take effect.
