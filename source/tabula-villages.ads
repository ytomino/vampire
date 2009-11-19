-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Villages is
	
	subtype Village_Id is String (1 .. 4);
	
	Invalid_Village_Id : constant Village_Id := "****";
	
	type Village_State is (Prologue, Opened, Epilogue, Closed);
	type Village_Time is (Daytime, Vote, Night);
	
	type Village is tagged;
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
		Village : not null access Villages.Village'Class;
		Item : in Root_Option_Item;
		Value : in String) is abstract;
	
	type Village is abstract tagged limited null record;
	
	procedure Iterate (
		Village : not null access constant Villages.Village;
		Process : not null access procedure (Item : in Root_Option_Item'Class))
		is abstract;
	function Option_Changed (Object : not null access constant Village) return Boolean;
	
end Tabula.Villages;
