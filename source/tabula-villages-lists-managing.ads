-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Villages.Lists.Managing is
	
	procedure Refresh_Village_List;
	procedure Clear_Village_List;
	function New_Village_Id return Village_Id;
	function Village_List return Village_Lists.Vector;
	procedure Make;
	
	function Exists(Id : Village_Id) return Boolean;
	
	function Short_Term_Village_Blocking return Boolean;
	
end Tabula.Villages.Lists.Managing;
