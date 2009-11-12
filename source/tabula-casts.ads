-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
package Tabula.Casts is
	
	type Sex_Kind is (Neutral, Male, Female);
	subtype Person_Sex is Sex_Kind range Male .. Female;
	
	type Person is tagged record
		Name : Ada.Strings.Unbounded.Unbounded_String;
		Work : Ada.Strings.Unbounded.Unbounded_String;
		Image : Ada.Strings.Unbounded.Unbounded_String;
		Sex : Person_Sex;
		Group : Integer;
	end record;
	
	Default_Person : constant Person := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Work => Ada.Strings.Unbounded.Null_Unbounded_String,
		Image => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Casts.Male,
		Group => 0);
	
	package People is new Ada.Containers.Vectors (Natural, Person);
	
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
	
	type Cast_Collection is limited record
		People : aliased Casts.People.Vector;
		Works : aliased Casts.Works.Vector;
	end record;
	
	procedure Exclude_Person (Cast : in out Casts.Cast_Collection; Name : String; Group : Integer);
	procedure Exclude_Work (Cast : in out Casts.Cast_Collection; Name : String);
	
end Tabula.Casts;
