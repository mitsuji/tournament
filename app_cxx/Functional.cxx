//
// g++ -o tournament-cxx-func-exe -std=c++98 app_cxx/Functional.cxx
// clang++ -o tournament-cxx-func-exe -std=c++98 app_cxx/Functional.cxx
//

#include <typeinfo>
#include <map>
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
  battle(const tournament& qualifi0, const tournament& qualifi1)
    :qualifi0(qualifi0), qualifi1(qualifi1)
  {}
  virtual ~battle() {}
};


const battle operator * (const tournament& t1, const tournament& t2)
{
  return battle(t1,t2);
}



int upper_id(int id){
  return id >> 1;
}

int lower_id(int id, int d){
  return (id << 1)+d;
}


typedef std::map<int,const char*> map_t;

const map_t toMap_ (const tournament& t, int id){
  map_t m;
  if(typeid(t) == typeid(const player&))
    {
      const player& p = dynamic_cast<const player&>(t);
      m[id] = p.name;
    }
  else if(typeid(t) == typeid(const battle&))
    {
      const battle& b = dynamic_cast<const battle&>(t);
      m[id] = NULL;
      const map_t& m0 = toMap_(b.qualifi0, lower_id(id,0));
      const map_t& m1 = toMap_(b.qualifi1, lower_id(id,1));
      m.insert(m0.begin(),m0.end());
      m.insert(m1.begin(),m1.end());
    }
  return m;
}
const map_t toMap (const tournament& t){
  return toMap_(t,1);
}


const map_t setWinner(const map_t& m, int id){
  const map_t::const_iterator it_name = m.find(id);
  const char* name = (*it_name).second;
  map_t n(m);
  n[upper_id(id)] = name;
  return n;
}

const map_t operator >> (const map_t& m, int id)
{
  return setWinner(m,id);
}


const std::string graph_(const map_t& m, int id, int d) {
  std::ostringstream out;

  const map_t::const_iterator it_name = m.find(id);
  const char* name = (*it_name).second;
  
  const int lower_id0 = lower_id(id,0);
  const int lower_id1 = lower_id(id,1);
  const map_t::const_iterator it_q0 = m.find(lower_id0);
  const map_t::const_iterator it_q1 = m.find(lower_id1);
  
  if(it_q0 == m.end() && it_q1 == m.end())
    {
      // player
      for(int i=0; i<d; ++i){ out << "  |"; }
      out << "-- [" << id << "]" << name << std::endl;
    }
  else
    {
      // battle
      for(int i=0; i<d; ++i){ out << "  |"; }
      if(name) {
	out << "--|[" << id << "](" << name << ")" << std::endl;
      } else {
	out << "--|[" << id << "]" << std::endl;
      }
      
      out << graph_(m,lower_id0,d+1);
      out << graph_(m,lower_id1,d+1);
      
      for(int i=0; i<d; ++i){ out << "  |"; }
      out << std::endl;
    }
  return out.str();
}
const std::string graph(const map_t& m) {
  return graph_(m,1,0);
}



int main (int argc, char** argv)
{

  {
    const map_t& m = toMap(
			   (
			    player("ゆづき")
			    *
			    player("はな")
			   )
			   *
			   player("たかまさ")
			  );
    
    std::cout << graph ( m >> 5 >> 3 );
  }

  {
    const map_t& m = toMap(
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
    
    std::cout << graph ( m >> 4 >> 7 >> 3 );
  }

  {
    const map_t& m = toMap(
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
			     player("たかまさ")
			     *
			     player("ゆみ")
			    )
			   )
			  );
    
    std::cout << graph ( m >> 14 >> 13 >> 6 >> 5 >> 3 );
  }
  
}  
