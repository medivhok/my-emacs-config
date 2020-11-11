all: site

site:
		emacs --batch --no-init-file --load publish.el --funcal medivhok-publish

clean:
		rm -rf public/*
