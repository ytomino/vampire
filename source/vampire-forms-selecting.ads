-- The Village of Vampire by YT, このソースコードはNYSLです
package Vampire.Forms.Selecting is
	
	function Select_Form (
		Query_Strings : Web.Query_Strings;
		Speeches_Per_Page : Tabula.Villages.Speech_Positive_Count'Base)
		return Root_Form_Type'Class;
	
end Vampire.Forms.Selecting;
