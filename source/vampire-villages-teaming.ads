-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Numerics.MT19937;
package Vampire.Villages.Teaming is
	
	type Role_Set is array (Person_Role) of Natural;
	type Role_Set_Array is array (Positive range <>) of Role_Set;
	
	function Possibilities (
		People_Count : Ada.Containers.Count_Type;
		Male_And_Female : Boolean;
		Execution : Execution_Mode;
		Formation : Formation_Mode;
		Unfortunate : Unfortunate_Mode;
		Monster_Side : Monster_Side_Mode)
		return Role_Set_Array;
	
	function Select_Set (
		Sets : Role_Set_Array;
		Appearance : Role_Appearances;
		Generator : aliased in out Ada.Numerics.MT19937.Generator)
		return Role_Set;
	
	procedure Shuffle (
		People : in out Villages.People.Vector;
		Victim : access Villages.Person_Role;
		Set : Role_Set;
		Generator : aliased in out Ada.Numerics.MT19937.Generator);
	
end Vampire.Villages.Teaming;
