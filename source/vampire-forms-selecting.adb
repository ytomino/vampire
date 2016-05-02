-- The Village of Vampire by YT, このソースコードはNYSLです
with Vampire.Forms.Full;
with Vampire.Forms.Mobile;
package body Vampire.Forms.Selecting is
	
	function Select_Form (
		Query_Strings : Web.Query_Strings;
		Speeches_Per_Page : Positive)
		return Root_Form_Type'Class is
	begin
		if Web.Element (Query_Strings, "b") = "k" then
			return Mobile.Form_Type'(Mobile.Create (Speeches_Per_Page));
		else
			return Full.Form_Type'(Full.Create);
		end if;
	end Select_Form;
	
end Vampire.Forms.Selecting;
