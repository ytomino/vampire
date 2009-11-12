-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Tabula.String_Lists;
package Tabula.Villages.Lists is

	subtype Village_Id is String(1..4);
	Invalid_Village_Id : constant Village_Id := "****";
	
	type Village_List_Item is record
		Id : Village_Id;
		Name : Ada.Strings.Unbounded.Unbounded_String;
		By : Ada.Strings.Unbounded.Unbounded_String;
		Day_Duration : Duration;
		Today : Natural;
		State : Villages.Village_State;
		People : String_Lists.List;
	end record;
	function "<" (L, R : Village_List_Item) return Boolean;
	
	package Village_Lists is new Ada.Containers.Vectors(Natural, Village_List_Item);

	function Joined(User_Id : String; List : Village_Lists.Vector; Long_Only : Boolean) return Boolean;
	function Created(User_Id : String; List : Village_Lists.Vector;
		Excluding : Village_Id) return Boolean;

end Tabula.Villages.Lists;
