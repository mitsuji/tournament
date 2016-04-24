//
// g++ -std=c++98 t.cpp
// clang++ -std=c++98 t.cpp
//

#include <typeinfo>
#include <stdio.h>
#include <map>

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
  battle(const tournament& qualifi0, const tournament& qualifi1)
    :qualifi0(qualifi0), qualifi1(qualifi1)
  {}
  virtual ~battle() {}
};

const battle operator * (const tournament& l, const tournament& r)
{
  return battle(l,r);
}

std::map<int,const char*> toMap_ (const tournament& t, int id){
  std::map<int,const char*> m;
  if(typeid(t) == typeid(const player&))
    {
      const player& p = dynamic_cast<const player&>(t);
      m.insert(std::pair<int,const char*>(id,p.name));
    }
  else if(typeid(t) == typeid(const battle&))
    {
      const battle& b = dynamic_cast<const battle&>(t);
      m.insert(std::pair<int,const char*>(id,NULL));
      const std::map<int,const char*>& m0 = toMap_(b.qualifi0, (id << 1));
      const std::map<int,const char*>& m1 = toMap_(b.qualifi1, (id << 1)+1);
      m.insert(m0.begin(),m0.end());
      m.insert(m1.begin(),m1.end());
    }
  return m;
}
std::map<int,const char*> toMap (const tournament& t){
  return toMap_(t,1);
}


std::map<int,const char*> setWinner(const std::map<int,const char*>& m, int id){
  std::map<int, const char*>::const_iterator xit = m.find(id);
  const char* x = (*xit).second;
  std::map<int,const char*> nm(m);
  //  nm.insert(std::pair<int,const char*>((id >> 1),x));
  nm[(id >> 1)] = x;
  return nm;
}

const std::map<int,const char*> operator >> (const std::map<int,const char*>& m, int id)
{
  return setWinner(m,id);
}


void graph_( const std::map<int,const char*>& m, int id, int d) {
  std::map<int, const char*>::const_iterator xit = m.find(id);
  const char* x = (*xit).second;
  std::map<int, const char*>::const_iterator qit0 = m.find((id << 1));
  std::map<int, const char*>::const_iterator qit1 = m.find((id << 1)+1);
  if(qit0 != m.end() && qit1 != m.end()){

      for(int i=0; i<d; ++i){ printf("  |"); }
      if(x) {
	printf("--|[%d](%s)\n", id, x);
      } else {
	printf("--|[%d]\n", id);
      }
      
      graph_(m, (id << 1),d+1);
      graph_(m, (id << 1)+1,d+1);
      
      for(int i=0; i<d; ++i){ printf("  |"); }
      printf("\n");
    
  } else {
  
      for(int i=0; i<d; ++i){ printf("  |"); }
      printf("-- [%d]",id);
      printf("%s\n",x);

  }
}
void graph( const std::map<int,const char*>& m) {
  graph_(m,1,0);
}



int main ( int argc, char** argv)
{

  {

    const std::map<int, const char*>& m = toMap(
						(
						 player("ゆづき")
						 *
						 player("はな")
						)
						*
						player("たかまさ")
					       );
	  
    graph( m >> 4 >> 3 );
  }

  {
    const std::map<int, const char*>& m = toMap(
						(
						 player("ゆづき")
						 *
						 player("はな")
						)
						*
						(
						 player("ゆみ")
						 *
						 player("たかまさ")
						)
					       );
	  
    graph( m >> 4 >> 7 >> 3 );
  }

  {
    const std::map<int, const char*>& m = toMap(
						(
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
						  player("ゆみ")
						  *
						  player("たかまさ")
						 )
						)
					       );
	  
    graph( m >> 14 >> 13 >> 6 >> 5 >> 2 );
  }
  
}  