-- The Village of Vampire by YT, このソースコードはNYSLです
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
	
	-- 村データ
	
	type Village_Type is abstract tagged limited record
		Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		By : aliased Ada.Strings.Unbounded.Unbounded_String; -- 作成者
	end record;
	
	function Term (Village : Village_Type) return Village_Term is abstract;
	
	procedure Get_State (
		Village : in Village_Type;
		State : out Village_State;
		Today : out Natural) is -- Prologue = 0
		abstract;
	
	procedure Iterate_People (
		Village : in Village_Type;
		Process : not null access procedure (Item : in Person_Type'Class)) is
		abstract;
	
	procedure Iterate_Options (
		Village : in Village_Type;
		Process : not null access procedure (Item : in Root_Option_Item'Class)) is
		abstract;
	
	function Option_Changed (Village : Village_Type) return Boolean;
	
end Tabula.Villages;
