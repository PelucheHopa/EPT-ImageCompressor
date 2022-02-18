##
## EPITECH PROJECT, 2020
## nothing
## File description:
## nothing
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	imageCompressor

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)
	rm -f stack.yaml.lock
	rm -f wolfram.cabal
clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	rm -f stack.yaml.lock
	rm -f wolfram.cabal

re: fclean all

.PHONY: all clean fclean re
## n'affiche pas les commandes
.SILENT:

## ne compare pas les fichiers

##export xLDFLAGS
