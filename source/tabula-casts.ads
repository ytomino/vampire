-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
package Tabula.Casts is
	
	type Group is record
		Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		By : aliased Ada.Strings.Unbounded.Unbounded_String;
		Width : Integer;
		Height : Integer;
		Group : Integer;
	end record;
	
	Empty_Group : constant Group := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		By => Ada.Strings.Unbounded.Null_Unbounded_String,
		Width => 0,
		Height => 0,
		Group => 0);
	
	package Groups is new Ada.Containers.Vectors (Natural, Group);
	
	function Find (Container : Groups.Vector; Group : Integer)
		return Groups.Cursor;
	
	type Neutralable_Sex is (Neutral, Male, Female);
	subtype Person_Sex is Neutralable_Sex range Male .. Female;
	
	type Person is tagged record
		Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		Work : aliased Ada.Strings.Unbounded.Unbounded_String;
		Image : aliased Ada.Strings.Unbounded.Unbounded_String;
		Sex : Person_Sex;
		Group : Integer;
	end record;
	
	function Is_Empty (Item : Person) return Boolean;
	
	Empty_Person : constant Person := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Work => Ada.Strings.Unbounded.Null_Unbounded_String,
		Image => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Male,
		Group => 0);
	
	package People is new Ada.Containers.Vectors (Natural, Person);
	
	type Work is record
		Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		Sex : Neutralable_Sex;
		Nominated : Boolean;
	end record;
	
	function Is_Empty (Item : Work) return Boolean;
	
	Empty_Work : constant Work := (
		Name => Ada.Strings.Unbounded.Null_Unbounded_String,
		Sex => Neutral,
		Nominated => False);
	
	package Works is new Ada.Containers.Vectors (Natural, Work);
	
	function Find (Works : Casts.Works.Vector; Name : String)
		return Casts.Works.Cursor;
	
	type Cast_Collection is
		-- limited -- see tabula-casts-load.adb
	record
		Groups : aliased Casts.Groups.Vector;
		People : aliased Casts.People.Vector;
		Works : aliased Casts.Works.Vector;
	end record;
	
	procedure Exclude_Person (
		Cast : in out Cast_Collection;
		Name : in String;
		Group : in Integer);
	procedure Exclude_Work (
		Cast : in out Cast_Collection;
		Name : in String);
	
end Tabula.Casts;
