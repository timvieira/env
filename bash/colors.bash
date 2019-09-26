# echo in color
function red    { echo -e "\e[31m$@\e[0m"; }
function yellow { echo -e "\e[33m$@\e[0m"; }
function green  { echo -e "\e[32m$@\e[0m"; }
function blue   { echo -e "\e[34m$@\e[0m"; }
function purple { echo -e "\e[35m$@\e[0m"; }
function cyan   { echo -e "\e[36m$@\e[0m"; }

function bright_red    { echo -e "\e[31;1m$@\e[0m"; }
function bright_yellow { echo -e "\e[33;1m$@\e[0m"; }
function bright_green  { echo -e "\e[32;1m$@\e[0m"; }
function bright_blue   { echo -e "\e[34;1m$@\e[0m"; }
function bright_purple { echo -e "\e[35;1m$@\e[0m"; }
function bright_cyan   { echo -e "\e[36;1m$@\e[0m"; }
