#include <stdio.h>

typedef enum {
  player,
  battle
} tournament_type;

typedef union tournament_ tournament;
typedef union tournament_ {
  struct {
    tournament_type type;
  } unit;
  struct {
    tournament_type type;
    tournament* qualifi0;
    tournament* qualifi1;
    char* winner_name;
  } battle;
  struct {
    tournament_type type;
    char* name;
  } player;
} tournament ;


void init_player(tournament* p, char* name)
{
  p->player.type = player;
  p->player.name = name;
}

void init_battle(tournament* p, tournament* qualifi0, tournament* qualifi1)
{
  p->battle.type = battle;
  p->battle.qualifi0 = qualifi0;
  p->battle.qualifi1 = qualifi1;
  p->battle.winner_name = NULL;
}

void graph_(tournament* p, int d);
void graph(tournament* p)
{
  graph_(p,0);
}

void graph_(tournament* p, int d)
{
  switch(p->unit.type)
    {
    case player :
      {
	int i;
	for(i=0; i<d; ++i){ printf("  |"); }
	printf("-- ");
	printf("%s\n",p->player.name);
      }
      break;
    case battle :
      {
	int i;
	for(i=0; i<d; ++i){ printf("  |"); }
	printf("--|%s\n","");
	graph_(p->battle.qualifi0,d+1);
	graph_(p->battle.qualifi1,d+1);
	for(i=0; i<d; ++i){ printf("  |"); }
	printf("\n");
      }
      break;
    }
}


int main (int argc, char** argv)
{

  tournament yuzu; init_player(&yuzu, "ゆずき");
  tournament hana; init_player(&hana, "はな");
  tournament taka; init_player(&taka, "たかまさ");

  tournament semiFin; init_battle(&semiFin, &yuzu,    &hana);
  tournament fin;     init_battle(&fin,     &semiFin, &taka); 

  graph(&fin);

}
