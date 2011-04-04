-- The Village of Vampire by YT, このソースコードはNYSLです
package Vampire.Villages.Text is
	
	-- 配役
	
	function Name (Person : Tabula.Villages.Person_Type'Class) return String;
	
	function Image (Role : Requested_Role) return String;
	
	function Image (Role : Person_Role) return String;
	function Short_Image (Role : Person_Role) return String; -- 漢字一文字
	
	-- 参加
	
	function Join (Village : Village_Type; Message : Villages.Message)
		return String;
	function Escaped_Join (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 村抜け
	function Escape (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 舞台
	
	-- プロローグのメッセージ
	function Introduction (Village : Village_Type) return String;
	
	-- 一日目のメッセージ
	function Breakdown (Village : Village_Type) return String;
	
	-- 二日目から処刑がある場合のメッセージ
	function For_Execution_In_Second (Village : Village_Type) return String;
	
	-- 進行
	
	-- 吸血鬼の一覧(Breakdownの前)
	function Vampires (Village : Village_Type) return String;
	
	-- 一番最初の内訳開示(Breakdownの後)
	function Teaming (Village : Village_Type) return String;
	
	-- 使徒が吸血鬼を知る(Breakdownの後)
	function Servant_Knew_Message (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 治療結果
	function Doctor_Cure_Message (Village : Village_Type; Message : Villages.Message)
		return String;
	-- 調査結果
	function Detective_Survey_Message (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 誰が誰に投票したか(無記名投票では非公開)
	function Votes (
		Village : Village_Type;
		Day : Natural;
		Preliminary : Boolean;
		Player_Index : Person_Index'Base)
		return String;
	-- 集計結果
	function Votes_Totaled (
		Village : Village_Type;
		Day : Natural;
		Preliminary : Boolean;
		Executed: Person_Index'Base)
		return String;
	
	-- 夜間の会話が数奇な運命の村人に妨害された
	function Howling_Blocked (Village : Village_Type) return String;
	
	-- 観測結果
	function Astronomer_Observation_Message (Village : Village_Type; Message : Villages.Message)
		return String;
	-- 護衛結果
	function Hunter_Guard_Message (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 襲撃結果
	function Vampire_Murder_Message (
		Village : Village_Type;
		Message : Villages.Message;
		Executed : Person_Index'Base)
		return String;
	
	function Foreboding_About_Infection_In_First (Village : Village_Type) return String;
	
	-- 感染自覚
	function Awareness (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 遺体の一覧
	function Fatalities (Village : Village_Type; Day : Natural; Executed : Person_Index'Base)
		return String;
	-- 生存者の一覧
	function Survivors (Village : Village_Type; Day : Natural) return String;
	
	-- 妖魔の吸血鬼数感知
	function Gremlin_Sense (Village : Village_Type; Day : Natural) return String;
	
	-- 違和感
	function Sweetheart_Incongruity (Village : Village_Type; Message : Villages.Message)
		return String;
	-- 後追い
	function Sweetheart_Suicide (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- アクション
	
	function Action_Wake (Village : Village_Type; Message : Villages.Message)
		return String;
	function Action_Encourage (Village : Village_Type; Message : Villages.Message)
		return String;
	function Action_Vampire_Gaze (Village : Village_Type; Message : Villages.Message)
		return String;
	function Action_Vampire_Gaze_Blocked (Village : Village_Type; Message : Villages.Message)
		return String;
	
	-- 決着
	
	-- 役とID公開
	function People_In_Epilogure (Village : Village_Type) return String;
	
	-- 勝利陣営
	function Result_In_Epilogure (Village : Village_Type) return String;
	
end Vampire.Villages.Text;
