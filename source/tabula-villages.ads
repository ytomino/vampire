-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Villages is
	
	subtype Village_Id is String (1 .. 4);
	
	Invalid_Village_Id : constant Village_Id := "****";
	
	type Village_State is (Prologue, Opened, Epilogue, Closed);
	type Village_Time is (Daytime, Vote, Night);
	
end Tabula.Villages;
