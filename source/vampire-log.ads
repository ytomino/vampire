-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Villages.Lists;
private with Vampire.Villages.Village_IO;
package Vampire.Log is
	
	Type_Code : aliased constant String;
	
	function Load_Summary (
		List : Tabula.Villages.Lists.Village_List;
		Id : Tabula.Villages.Village_Id)
		return Tabula.Villages.Lists.Village_Summary;
	procedure Create_Log (
		List : Tabula.Villages.Lists.Village_List;
		Id : in Tabula.Villages.Village_Id);
	procedure Create_Index (
		Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
		Update : in Boolean);
	
private
	
	Type_Code : aliased constant String := Villages.Village_IO.Yaml_Type;
	
end Vampire.Log;
