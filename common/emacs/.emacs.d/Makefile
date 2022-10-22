# Defines phony targets all and clean
.PHONY = all compile clean

# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch -L ./site-lisp

# Remove command
RM = rm -f

# Define Compile Command (CC)
# Call batch-byte-compile function: -f
# CC := -f batch-native-compile
CC := -f batch-byte-compile

# Emacs user directory
EMACS_USER_DIR = $(HOME)/.emacs.d

# Tangle script
TANGLE_SCRIPT = ${EMACS_USER_DIR}/tangle.el

# Emacslisp files to compile
EL_FILES = ${EMACS_USER_DIR}/init.el ${EMACS_USER_DIR}/early-init.el $(wildcard ${EMACS_USER_DIR}/site-lisp/*.el)

# default target
all: site-lisp-autoloads tangle compile

# run tangle script (org related)
# the script will tangle the source code blocks from emacs-config.org
# to compose the init.el and early-init.el final script
tangle:
	${EMACS} -l ${TANGLE_SCRIPT} -f tangle-config

site-lisp-autoloads:
	${EMACS} -l ${TANGLE_SCRIPT} -f generate-autoload-site-lisp

# Compile init.el and early-init.el
compile:
	 ${EMACS} ${CC} ${EL_FILES}

# Remove all byte compiled files
clean:
	${RM} *.elc
