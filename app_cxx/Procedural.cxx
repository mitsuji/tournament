//
// g++ -o tournament-cxx-proc-exe -std=c++98 app_cxx/Procedural.cxx
// clang++ -o tournament-cxx-proc-exe -std=c++98 app_cxx/Procedural.cxx
//

#include <typeinfo>
#include <sstream>
#include <iostream>

struct tournament {
  virtual ~tournament() {}
};


struct player : tournament {
  const char* name;
  player(const char* name)
    :name(name)
  {}
  virtual ~player() {}
};


struct battle : tournament {
  const tournament& qualifi0;
  const tournament& qualifi1;
  const char* winner_name;
  battle(const tournament& qualifi0, const tournament& qualifi1)
    :qualifi0(qualifi0), qualifi1(qualifi1), winner_name(NULL)
  {}
  virtual ~battle() {}
};


void setWinner(battle& t, const player& p){
  t.winner_name = p.name;
}


const std::string graph_(const tournament& t, int d) {
  std::ostringstream out;
  if(typeid(t) == typeid(const player&))
    {
      const player& p = dynamic_cast<const player&>(t);
      
      for(int i=0; i<d; ++i){ out << "  |"; }
      out << "-- " << p.name << std::endl;
    }
  else if(typeid(t) == typeid(const battle&))
    {
      const battle& b = dynamic_cast<const battle&>(t);

      for(int i=0; i<d; ++i){ out << "  |"; }
      if(b.winner_name) {
	out << "--|(" << b.winner_name << ")" << std::endl;
      } else {
	out << "--|" << std::endl;
      }
      
      out << graph_(b.qualifi0,d+1);
      out << graph_(b.qualifi1,d+1);
      
      for(int i=0; i<d; ++i){ out << "  |"; }
      out << std::endl;
    }
  return out.str();
}
const std::string graph(const tournament& t) {
  return graph_(t,0);
}



int main (int argc, char** argv)
{
  
  {
    player yuzu("ゆづき");
    player hana("はな");
    player taka("たかまさ");
    
    battle semiFin(yuzu,hana);
    battle fin(semiFin,taka);

    setWinner(semiFin, hana);
    setWinner(fin, taka);

    std::cout << graph(fin);
  }

  {
    player yuzu("ゆづき");
    player hana("はな");
    player taka("たかまさ");
    player yumi("ゆみ");

    battle semiFin1(yuzu,hana);
    battle semiFin2(taka,yumi);
    battle fin(semiFin1,semiFin2);

    setWinner(semiFin1, yuzu);
    setWinner(semiFin2, yumi);
    setWinner(fin, yumi);
    
    std::cout << graph(fin);
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
    
    std::cout << graph(fin);
  }

}  
