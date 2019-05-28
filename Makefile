all: install_summon summon

clean:
	rm -rf ~/git/summon

install_summon:
	git clone https://gitlab.com/semente/summon $$HOME/git/summon; \
	cd $$HOME/git/summon; \
	chmod 755 $$HOME/git/summon/summon.sh; \

summon:
	sh ~/git/summon/summon.sh
