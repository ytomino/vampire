-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Tabula.String_Lists;
package Tabula.Villages.Lists is
	
	-- managing
	
	function Exists (Id : Village_Id) return Boolean;
	function New_Village_Id return Village_Id;
	
	-- list
	
	type Village_List_Item is record
		Id : Village_Id;
		Name : Ada.Strings.Unbounded.Unbounded_String;
		By : Ada.Strings.Unbounded.Unbounded_String;
		Day_Duration : Duration;
		Today : Natural;
		State : Village_State;
		People : String_Lists.List;
	end record;
	function "<" (L, R : Village_List_Item) return Boolean;
	
	package Village_Lists is new Ada.Containers.Vectors(Natural, Village_List_Item);
	
	function Joined(
		User_Id : String;
		List : Village_Lists.Vector;
		Long_Only : Boolean) return Boolean;
	function Created(
		User_Id : String;
		List : Village_Lists.Vector;
		Excluding : Village_Id) return Boolean;
	
	function Closed_Only_Joined_Count (
		User_Id : String;
		List : Village_Lists.Vector;
		Escaped : Boolean) return Natural;
	
	-- I/O
	procedure Make_Log_Index (List : not null access constant Village_Lists.Vector);
	procedure Make_RSS (List : not null access constant Village_Lists.Vector);
	
	function Village_List (
		Load_Info : not null access function (Id : in Village_Id) return Village_List_Item)
		return Village_Lists.Vector;
	
	procedure Update_Village_List (
		Remake_All : Boolean := False;
		Load_Info : not null access function (Id : in Village_Id) return Village_List_Item;
		Create_Log : not null access procedure (Id : in Village_Id));
	
	-- 短期村作成禁止
	function Short_Term_Village_Blocking return Boolean;
	
end Tabula.Villages.Lists;
