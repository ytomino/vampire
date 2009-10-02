-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Vectors;
package Tabula.Villages.Casts is
	
	type Work is record
		Name : Ada.Strings.Unbounded.Unbounded_String;
		Sex : Sex_Kind;
		Nominated : Boolean;
	end record;
	
	Default_Work : constant Work := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Neutral,
		Nominated => False);
	
	package Works is new Ada.Containers.Vectors (Natural, Work);
	
	type Cast_Type is limited record
		People : aliased Villages.People.Vector;
		Works : aliased Casts.Works.Vector;
	end record;
	
	procedure Exclude_Taken(Cast : in out Casts.Cast_Type; Village : Village_Type);
	
end Tabula.Villages.Casts;
