-- The Village of Vampire by YT, このソースコードはNYSLです
function Vampire.Forms.Select_Form (
	Query_Strings : Web.Query_Strings;
	Speeches_Per_Page : Positive)
--	return Root_Form_Type'Class;
	return access Root_Form_Type'Class;
