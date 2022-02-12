instruments = putt.ins splash.ins
#instruments = $(wildcard %.ins)

sfx = $(patsubst %.ins,%.b,$(instruments))

all : $(sfx)
	
$(sfx) : 
	ins2snd2 $(patsubst %.b,%.ins,$@) $@ -b




