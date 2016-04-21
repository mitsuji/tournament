import Glibc

protocol Tournament {
	 func graph(d:Int) -> String
}

class Battle: Tournament {
      var qualifi0: Tournament
      var qualifi1: Tournament
      var winner_name: String?
      init ( qualifi0: Tournament, qualifi1: Tournament) {
      	   self.qualifi0 = qualifi0
      	   self.qualifi1 = qualifi1
      }
      func graph(d:Int) -> String {
      	   var s = ""
	   
	   for _ in 0..<d {
	       s += "  |"
           }
           s += "--|" + "\n"
	   
	   s += qualifi0.graph(d+1)
	   s += qualifi1.graph(d+1)

	   for _ in 0..<d {
	       s += "  |"
           }
           s += "\n"

	   return s
      }
}

class Player: Tournament {
      var name: String
      init ( name: String) {
      	   self.name = name
      }
      func graph(d:Int) -> String {
      	   var s = ""
	   for _ in 0..<d {
	       s += "  |"
           }
           s += "-- " + name + "\n"
	   return s
      }
}


func *(q0:Tournament, q1:Tournament) -> Tournament {
     return Battle(qualifi0:q0,qualifi1:q1)
}


let fin =
    	(
		Player(name:"ゆづき")
		*
		Player(name:"はな")
	)
	*
	Player(name:"たかまさ")


print(fin.graph(0))

