-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Tabula.Casts;
package Tabula.Villages is
	
	-- 村のID
	
	subtype Village_Id is String (1 .. 4);
	
	Invalid_Village_Id : constant Village_Id := "****";
	
	-- 村の状態
	
	type Village_Term is (Short, Long); -- 短期 or 長期
	
	type Village_State is (Prologue, Playing, Epilogue, Closed);
	
	-- 参加者
	
	type Person_Type is new Casts.Person with record
		Id : aliased Ada.Strings.Unbounded.Unbounded_String;
	end record;
	
	subtype Person_Index is Natural;
	No_Person : constant Person_Index'Base := -1;
	
	function Same_Id_And_Figure (Left, Right : Person_Type'Class) return Boolean;
	
	-- メッセージ
	
	subtype Message_Index is Natural;
	type Message_Range_Type is record
		First : Message_Index;
		Last : Message_Index'Base;
	end record;
	
	-- オプション
	
	type Village_Type is tagged;
	
	type Root_Option_Item is abstract tagged limited null record;
	
	function Available (Item : Root_Option_Item) return Boolean is abstract;
	function Name (Item : Root_Option_Item) return String is abstract;
	function Changed (Item : Root_Option_Item) return Boolean is abstract;
	procedure Iterate (
		Item : in Root_Option_Item;
		Process : not null access procedure (
			Value : in String;
			Selected : in Boolean;
			Message : in String;
			Unrecommended : in Boolean)) is abstract;
	procedure Change (
		Village : in out Village_Type'Class;
		Item : in Root_Option_Item;
		Value : in String) is abstract;
	
	-- 村
	
	type Village_Type is abstract tagged limited record
		Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		By : aliased Ada.Strings.Unbounded.Unbounded_String; -- 作成者
		Face_Group : Integer := 0; -- 顔絵セット
		Face_Width : Integer := 0;
		Face_Height : Integer := 0;
	end record;
	
	function Term (Village : Village_Type) return Village_Term is abstract;
	
	procedure Get_State (
		Village : in Village_Type;
		State : out Village_State;
		Today : out Natural) is abstract; -- Prologue = 0
	
	procedure Iterate_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Person_Type'Class)) is abstract;
	
	procedure Iterate_Escaped_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Person_Type'Class)) is abstract;
	
	function Message_Range (
		Village : Village_Type;
		Day : Natural)
		return Message_Range_Type is abstract;
	
	function Recent_Only_Message_Range (
		Village : Village_Type;
		Day : Natural;
		Now : Ada.Calendar.Time)
		return Message_Range_Type is abstract;
	
	procedure Iterate_Options (
		Village : in Village_Type;
		Process : not null access procedure (Item : in Root_Option_Item'Class)) is
		abstract;
	
	function Option_Changed (Village : Village_Type) return Boolean;
	
	-- 参加状況
	function Joined (Village : Village_Type; User_Id : String)
		return Person_Index'Base;
	
	function Already_Joined_As_Another_Sex (
		Village : Village_Type;
		User_Id : String;
		Sex : Casts.Person_Sex)
		return Boolean;
	function Male_And_Female (Village : Village_Type) return Boolean;
	
	-- 既に取られているものを除外
	procedure Exclude_Taken (
		Cast : in out Casts.Cast_Collection;
		Village : in Village_Type);
	
end Tabula.Villages;
