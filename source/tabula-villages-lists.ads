-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
package Tabula.Villages.Lists is
	
	package User_Lists is
		new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
	
	type Village_Summary is record
		Type_Code : aliased Ada.Strings.Unbounded.Unbounded_String; -- YAML type
		Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		By : aliased Ada.Strings.Unbounded.Unbounded_String;
		Day_Duration : Duration;
		Today : Natural;
		State : Village_State;
		People : User_Lists.List;
	end record;
	
	package Summary_Maps is new Ada.Containers.Ordered_Maps (Village_Id, Village_Summary);
	
	type Villages_List (<>) is limited private;
	
	type Load_Summary_Function is access function (
		List : Villages_List;
		Id : Village_Id)
		return Village_Summary;
	type Create_Log_Procedure is access procedure (
		List : Villages_List;
		Id : in Villages.Village_Id);
	type Create_Index_Procedure is access procedure (
		Summaries : in Summary_Maps.Map;
		Update : in Boolean);
	
	function Create (
		Data_Directory : not null Static_String_Access;
		HTML_Directory : not null Static_String_Access;
		Blocking_Short_Term_File_Name : not null Static_String_Access;
		Cache_File_Name : not null Static_String_Access;
		Load_Summary : not null Load_Summary_Function;
		Create_Log : not null Create_Log_Procedure;
		Create_Index : not null Create_Index_Procedure)
		return Villages_List;
	
	-- 村ID
	
	function File_Name (List : Villages_List; Id : Village_Id) return String;
	function HTML_File_Name (List : Villages_List; Id : Village_Id; Day : Natural) return String;
	
	function Exists (List : Villages_List; Id : Village_Id) return Boolean;
	function New_Village_Id (List : Villages_List) return Village_Id;
	
	-- 問い合わせ
	
	procedure Get_Summaries (List : in out Villages_List; Result : out Summary_Maps.Map);
	
	-- あるユーザーが村を作っているか
	function Exists_Opened_By (
		Summaries : Summary_Maps.Map;
		User_Id : String;
		Excluding : Village_Id := Invalid_Village_Id)
		return Boolean;
	-- 参加数
	type Village_State_Set is array (Village_State) of Boolean;
	function Count_Joined_By (
		Summaries : Summary_Maps.Map;
		User_Id : String;
		Filter : Village_State_Set;
		Long_Only : Boolean := False;
		Including_Escaped : Boolean := False)
		return Natural;
	
	-- 更新
	
	procedure Update (
		List : in out Villages_List;
		Id : Village_Id;
		Summary : Village_Summary);
	
	procedure Refresh (List : in out Villages_List); -- 全部を再作成
	
	-- 短期村作成禁止
	function Blocking_Short_Term (List : Villages_List) return Boolean;
	
private
	
	type Villages_List is limited record
		Data_Directory : not null Static_String_Access;
		HTML_Directory : not null Static_String_Access;
		Blocking_Short_Term_File_Name : not null Static_String_Access;
		Cache_File_Name : not null Static_String_Access;
		Load_Summary : not null Load_Summary_Function;
		Create_Log : not null Create_Log_Procedure;
		Create_Index : not null Create_Index_Procedure;
		Map : aliased Summary_Maps.Map;
		Map_Read : Boolean;
	end record;
	
end Tabula.Villages.Lists;
