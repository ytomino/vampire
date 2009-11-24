-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Vampires.Villages.Teaming is
	
	type Role_Set is array (Person_Role) of Natural;
	type Role_Set_Array is array (Positive range <>) of Role_Set;
	
	function Possibilities (
		People_Count : Ada.Containers.Count_Type;
		Male_And_Female : Boolean;
		Execution : Execution_Mode;
		Teaming : Teaming_Mode;
		Monster_Side : Monster_Side_Mode;
		Appearance : Role_Appearances) return Role_Set_Array;
	
end Tabula.Vampires.Villages.Teaming;
