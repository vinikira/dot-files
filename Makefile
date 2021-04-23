GIT_DIR = $$HOME/git
SUMMON_DIR := ${GIT_DIR}/summon

.PHONY=all clean install_summon summon

all: install_summon summon

clean:
	rm -rf ${SUMMON_DIR}

install_summon:
	mkdir -p ${GIT_DIR}
	if [ ! -d "${SUMMON_DIR}" ]; then git clone https://gitlab.com/semente/summon ${SUMMON_DIR}; fi
	chmod 755 ${SUMMON_DIR}/summon.sh

summon:
	bash ${SUMMON_DIR}/summon.sh $(workspace)
