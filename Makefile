app=OU-UNSA-ML

.PHONY: push
push:
	git add .
	git commit -m "Generated by Makefile"
	git push origin main 