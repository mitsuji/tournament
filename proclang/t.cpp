//
// g++ -std=c++98 t.cpp
// clang++ -std=c++98 t.cpp
//

#include <typeinfo>
#include <stdio.h>

struct tournament {
#ifndef __GNUC__ // for gcc's strange behavior
  virtual ~tournament() {}
#else
  virtual void marker() const = 0;
#endif
};

struct player : tournament {
  const char* name;
  player(const char* name)
    :name(name)
  {}
  virtual ~player() {}
#ifdef __GNUC__ // for gcc's strange behavior
  virtual void marker() const {}
#endif
};

struct battle : tournament {
  const tournament& qualifi0;
  const tournament& qualifi1;
  const char* winner_name;
  battle(const tournament& qualifi0, const tournament& qualifi1)
    :qualifi0(qualifi0), qualifi1(qualifi1), winner_name(NULL)
  {}
  virtual ~battle() {}
#ifdef __GNUC__ // for gcc's strange behavior
  virtual void marker() const {}
#endif
};

const battle operator * (const tournament& l, const tournament& r)
{
  return battle(l,r);
}

void setWinner(battle& t, const player& p){
  t.winner_name = p.name;
}

void graph_( const tournament& t, int d) {
  if(typeid(t) == typeid(const player&))
    {
      const player& p = dynamic_cast<const player&>(t);
      
      for(int i=0; i<d; ++i){ printf("  |"); }
      printf("-- ");
      printf("%s\n",p.name);
    }
  else if(typeid(t) == typeid(const battle&))
    {
      const battle& b = dynamic_cast<const battle&>(t);

      for(int i=0; i<d; ++i){ printf("  |"); }
      if(b.winner_name) {
	printf("--|(%s)\n", b.winner_name);
      } else {
	printf("--|\n");
      }
      
      graph_(b.qualifi0,d+1);
      graph_(b.qualifi1,d+1);
      
      for(int i=0; i<d; ++i){ printf("  |"); }
      printf("\n");
    }
}
void graph( const tournament& t) {
  graph_(t,0);
}


int main ( int argc, char** argv)
{
  
  {
    player yuzu("ゆづき");
    player hana("はな");
    player taka("たかまさ");
    battle semiFin(yuzu,hana);
    battle fin(semiFin,taka);

    setWinner(semiFin, hana);
    setWinner(fin, taka);
    
    graph(fin);
  }

  {
    player yuzu("ゆづき");
    player hana("はな");
    player taka("たか");
    player yumi("ゆみ");
    battle semiFin1(yuzu,hana);
    battle semiFin2(taka,yumi);
    battle fin(semiFin1,semiFin2);

    setWinner(semiFin1, yuzu);
    setWinner(semiFin2, yumi);
    setWinner(fin, yumi);
    
    graph(fin);
  }

  {
    player taro("たろう");
    player jiro("じろう");
    player yuzu("ゆづき");
    player hana("はな");
    player taka("たかまさ");
    player yumi("ゆみ");
    battle semiFin1(taro,jiro);
    battle semisemiFin1(yuzu,hana);
    battle semisemiFin2(taka,yumi);
    battle semiFin2(semisemiFin1,semisemiFin2);
    battle fin(semiFin1,semiFin2);

    setWinner(semiFin1, jiro);
    setWinner(semisemiFin1, hana);
    setWinner(semisemiFin2, taka);
    setWinner(semiFin2, hana);
    setWinner(fin, hana);
    
    graph(fin);
  }


  
  /*
  {
    const tournament& fin = (
			     player("ゆづき")
			     *
			     player("はな")
			    )
                            *
                            player("たかまさ");

    graph(fin);
  }

  {
    const tournament& fin = (
			     player("ゆづき")
			     *
			     player("はな")
			    )
                            *
                            (
                             player("ゆみ")
			     *
                             player("たかまさ")
			    );

    graph(fin);
  }

  {
    const tournament& fin = (
			     player("たろう")
			     *
			     player("じろう")
			    )
                            *
                            (
			     (
			      player("ゆづき")
			      *
			      player("はな")
			     )
			     *
			     (
			      player("たかまさ")
			      *
			      player("ゆみ")
			     )
			    );
    graph(fin);
  }
  */
  
}  
