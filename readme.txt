The Village of Vampire by YT

*設置

root/
	vampire.exe or vampire.cgi (実行可能属性)
	cast
	face.html
	readme.html
	style.css
	template-*.html
	image/
		*.png
	temp/
		.htaccess (deny from all)
	users/
		.htaccess (deny from all)
	villages/
		data/
			.htaccess (deny from all)

設置したら真っ先に"administrator"というユーザーを作ってください。
それでログオンすると新しい村を作ることができます。
また、進行中の村に地の文を書き込めます。"administrator"での参加はできません。

*ビルドに必要なもの

gcc 4.4.1を使わせていただいています。
http://gcc.gnu.org/

乱数アルゴリズムにMT19937を使わせていただいています。
http://www.math.keio.ac.jp/matumoto/emt.html
BSDLです。ライセンス文書はase-numerics-mt19937.adsの先頭にコメントで。

dyayamlを使っています。
http://panathenaia.halfmoon.jp/alang/dyayaml.html
YAMLはhttp://yaml.org/を参照してくださいと言いたいのですがdyayamlはサブセットかつ幾つか独自仕様があります。まあ適当に。
(いずれlibyamlに移行したい)

それ以外で私が書いたコードはNYSL(煮るなり焼くなり好きにしろライセンス)ということにしてください。
http://www.kmonos.net/nysl/

絵は…再配布禁止で。
