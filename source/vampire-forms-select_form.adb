-- The Village of Vampire by YT, このソースコードはNYSLです
with Vampire.Forms.Full;
with Vampire.Forms.Mobile;
function Vampire.Forms.Select_Form (Query_Strings : in Web.Query_Strings)
	return Root_Form_Type'Class is
begin
	if Web.Element (Query_Strings, "b") = "k" then
		return Mobile.Form_Type'(Mobile.Create);
	else
		return Full.Form_Type'(Full.Create);
	end if;
end Vampire.Forms.Select_Form;

