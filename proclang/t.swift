import Glibc

protocol Tournament {}

class Battle: Tournament {
      var qualifi0: Tournament
      var qualifi1: Tournament
      var winner_name: String?
      init ( qualifi0: Tournament, qualifi1: Tournament) {
      	   self.qualifi0 = qualifi0
      	   self.qualifi1 = qualifi1
      }
}

class Player: Tournament {
      var name: String
      init ( name: String) {
      	   self.name = name
      }
}


func *(q0:Tournament, q1:Tournament) -> Tournament {
     return Battle(qualifi0:q0,qualifi1:q1)
}


var fin =
    	(
		Player(name:"ゆづき")
		*
		Player(name:"はな")
	)
	*
	Player(name:"たかまさ")


print("こんにちは")
