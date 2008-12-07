-- The Village of Vampire by YT, このソースコードはNYSLです
with Ase.Numerics.MT19937;
procedure Tabula.Villages.Shuffle(
	People : in out Villages.Person_Array;
	Victim : access Villages.Person_Role;
	Teaming : Villages.Teaming;
	Monster_Side : Villages.Monster_Side;
	Appearance : Villages.Role_Appearances;
	Generator : in out Ase.Numerics.MT19937.Generator);
