======================
The Village of Vampire
======================

.. class:: align-right

*by YT*

設置
====

- root/
  
  - vampire.exe or vampire.cgi (実行可能属性)
  - cast
  - face.html
  - readme.html
  - style.css
  - template-\*.html
  - image/
    
    - \*.png
    
  - temp/
    
    - .htaccess (deny from all)
    
  - users/
    
    - .htaccess (deny from all)
    - +A/
    
  - villages/
    
    - data/
      
      - .htaccess (deny from all)

設置したら真っ先に"administrator"というユーザーを作ってください。
それでログオンすると新しい村を作ることができます。
また、進行中の村に地の文を書き込めます。"administrator"での参加はできません。

ビルドに必要なもの
==================

gcc 6を使わせていただいています。
 http://gcc.gnu.org/

パスワードを保存するときのハッシュ関数にOpenSSLを使わせていただいています。
 http://www.openssl.org/

データの保存のためにlibyamlを使わせていただいています。
 http://pyyaml.org/wiki/LibYAML

C言語のヘッダーをAdaに変換するために"headmaster"(拙作)を使用しています。
 https://github.com/ytomino/headmaster

Adaの標準ライブラリには、gcc標準のものではなくdrake(拙作)を使用しています。
 https://github.com/ytomino/drake

ライセンス
==========

autherさんの顔絵セット、Juna様の人狼審問α2.5セット、sinさんの文明開化セットは\
元々の配布条件に従ってください。

私の絵は……再配布禁止で。

The Village of Vampire本体のソースコードのライセンス
----------------------------------------------------

http://panathenaia.halfmoon.jp/NYSL.TXT [#]_

.. [#] NYSLについては http://www.kmonos.net/nysl/ を参照してください。

license of MT19937
------------------

.. code-block ::

 /*
    A C-program for MT19937, with initialization improved 2002/1/26.
    Coded by Takuji Nishimura and Makoto Matsumoto.
 
    Before using, initialize the state by using init_genrand(seed)  
    or init_by_array(init_key, key_length).
 
    Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
    All rights reserved.                          
 
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
 
      1. Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.
 
      2. Redistributions in binary form must reproduce the above copyright
         notice, this list of conditions and the following disclaimer in the
         documentation and/or other materials provided with the distribution.
 
      3. The names of its contributors may not be used to endorse or promote 
         products derived from this software without specific prior written 
         permission.
 
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 
 
    Any feedback is very welcome.
    http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
    email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
 */

license of OpenSSL
------------------

.. code-block ::

 /* ====================================================================
  * Copyright (c) 1998-2008 The OpenSSL Project.  All rights reserved.
  *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  *
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  *
  * 2. Redistributions in binary form must reproduce the above copyright
  *    notice, this list of conditions and the following disclaimer in
  *    the documentation and/or other materials provided with the
  *    distribution.
  *
  * 3. All advertising materials mentioning features or use of this
  *    software must display the following acknowledgment:
  *    "This product includes software developed by the OpenSSL Project
  *    for use in the OpenSSL Toolkit. (http://www.openssl.org/)"
  *
  * 4. The names "OpenSSL Toolkit" and "OpenSSL Project" must not be used to
  *    endorse or promote products derived from this software without
  *    prior written permission. For written permission, please contact
  *    openssl-core@openssl.org.
  *
  * 5. Products derived from this software may not be called "OpenSSL"
  *    nor may "OpenSSL" appear in their names without prior written
  *    permission of the OpenSSL Project.
  *
  * 6. Redistributions of any form whatsoever must retain the following
  *    acknowledgment:
  *    "This product includes software developed by the OpenSSL Project
  *    for use in the OpenSSL Toolkit (http://www.openssl.org/)"
  *
  * THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
  * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
  * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
  * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
  * OF THE POSSIBILITY OF SUCH DAMAGE.
  * ====================================================================
  *
  * This product includes cryptographic software written by Eric Young
  * (eay@cryptsoft.com).  This product includes software written by Tim
  * Hudson (tjh@cryptsoft.com).
  *
  */

license of libyaml
------------------

.. code-block ::

 Copyright (c) 2006 Kirill Simonov
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
