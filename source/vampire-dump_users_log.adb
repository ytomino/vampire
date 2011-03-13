-- The Village of Vampire by YT, このソースコードはNYSLです
-- ユーザーログ表示ツールです、CGIとして公開しないでください
with Tabula.Users.Lists.Dump;
with Vampire.Configurations;
procedure Vampire.Dump_Users_Log is
begin
	Tabula.Users.Lists.Dump (
		Users_Directory => Configurations.Users_Directory'Access,
		Users_Log_File_Name => Configurations.Users_Log_File_Name'Access);
end Vampire.Dump_Users_Log;
