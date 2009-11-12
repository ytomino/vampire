-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Numerics.MT19937;
procedure Tabula.Vampire.Villages.Shuffle(
	People : in out Villages.People.Vector;
	Victim : access Villages.Person_Role;
	Teaming : Villages.Teaming;
	Monster_Side : Villages.Monster_Side;
	Appearance : Villages.Role_Appearances;
	Generator : not null access Ada.Numerics.MT19937.Generator);
