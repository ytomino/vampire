-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Numerics.MT19937;
procedure Tabula.Vampires.Villages.Shuffle(
	People : in out Villages.People.Vector;
	Victim : access Villages.Person_Role;
	Teaming : in Teaming_Mode;
	Monster_Side : in Monster_Side_Mode;
	Appearance : Villages.Role_Appearances;
	Generator : not null access Ada.Numerics.MT19937.Generator);
