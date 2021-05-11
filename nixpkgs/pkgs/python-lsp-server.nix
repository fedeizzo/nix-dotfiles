{ lib
, python38Packages
}:

let
  python-lsp-jsonrpc = python38Packages.buildPythonPackage rec {
    pname = "python-lsp-jsonrpc";
    version = "1.0.0";

    # disabled = pythonOlder "3.6";

    src = python38Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-e+wXBzPbYo01Buo6Uoj/dqozxwIV7SI6vbDZXpV2YL0=";
    };

    nativeBuildInputs = with python38Packages; [ ujson ];
    checkInputs = with python38Packages; [ pytest ];
  };
in
python38Packages.buildPythonApplication rec {
  pname = "python-lsp-server";
  version = "1.0.1";

  src = python38Packages.fetchPypi {
    inherit pname version;
    sha256 = "sha256-Dz1nQFc07Yf2XjCn1+cGlTQeP/3KTj0HxNCbMROj5dY=";
  };
  doCheck = false;

  buildInputs = with python38Packages; [
    jedi
    pluggy
    ujson
    setuptools
    rope
    pyflakes
    mccabe
    pycodestyle
    pydocstyle
  ] ++ [ python-lsp-jsonrpc ];

  propagatedBuildInputs = buildInputs;

  checkInputs = with python38Packages; [
    autopep8
    flake8
    mccabe
    pycodestyle
    pydocstyle
    pyflakes
    pylint
    rope
    yapf
    pytest
    pylint
    pytestcov
    coverage
    numpy
    pandas
    matplotlib
    pyqt5
    flaky
  ];
}
