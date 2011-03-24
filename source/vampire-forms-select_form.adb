-- The Village of Vampire by YT, このソースコードはNYSLです
with Vampire.Forms.Full;
with Vampire.Forms.Mobile;
function Vampire.Forms.Select_Form (
	Query_Strings : Web.Query_Strings;
	Speeches_Per_Page : Positive)
	return Root_Form_Type'Class is
begin
	if Web.Element (Query_Strings, "b") = "k" then
		return Mobile.Create (Speeches_Per_Page);
	else
		return Full.Create;
	end if;
end Vampire.Forms.Select_Form;

