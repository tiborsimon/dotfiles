#!/usr/bin/env bats

load ../utils/dotfiles.lib


LINK_TARGET='../../sandbox/a/file'
LINK_NAME='sandbox/b/link'
TARGET_CONTENT='content'
OTHER_FILE_CONTENT='other'


# Create the test sandbox with the directory structure and a test file
setup () {
  mkdir -p sandbox/a/
  mkdir -p sandbox/b/
  echo ${TARGET_CONTENT} > sandbox/a/file
}

# Clean up the test sandbox
teardown () {
  rm -rf sandbox
}


@test "fresh link can be created" {
  run link_file  ${LINK_TARGET} ${LINK_NAME}
  [ $status -eq 0 ]
  [ $output = 'linked' ]
  [ $(readlink ${LINK_NAME}) = ${LINK_TARGET} ]
  [ $(cat ${LINK_NAME}) = ${TARGET_CONTENT} ]
}

@test "link already exists with the same content, linking should be skipped" {
  ln -s ${LINK_TARGET} ${LINK_NAME}
  run link_file ${LINK_TARGET} ${LINK_NAME}
  [ $status -eq 0 ]
  [ $output = 'exists' ]
}

@test "link already exists with different content, no policy specified, function should return" {
  echo ${OTHER_FILE_CONTENT} > ${LINK_NAME}
  run link_file ${LINK_TARGET} ${LINK_NAME}
  [ $status -eq 1 ]
  [ "$output" = '' ]
  [ $(cat ${LINK_NAME}) = ${OTHER_FILE_CONTENT} ]
}

@test "link exist, backup policy specified" {
  echo ${OTHER_FILE_CONTENT} > ${LINK_NAME}
  run link_file ${LINK_TARGET} ${LINK_NAME} 'b'
  [ $status -eq 0 ]
  [ ${lines[0]} = 'moved' ]
  [ ${lines[1]} = 'linked' ]
  [ -f ${LINK_NAME}.backup ]  # old file exists as a backup
  [ $(cat ${LINK_NAME}.backup) = ${OTHER_FILE_CONTENT} ]
  [ -L ${LINK_NAME} ]  # file linked
  [ $(cat ${LINK_NAME}) = ${TARGET_CONTENT} ]
}

@test "link exist, overwrite policy specified" {
  echo ${OTHER_FILE_CONTENT} > ${LINK_NAME}
  run link_file ${LINK_TARGET} ${LINK_NAME} 'o'
  [ $status -eq 0 ]
  [ ${lines[0]} = 'deleted' ]
  [ ${lines[1]} = 'linked' ]
  [ ! -f ${LINK_NAME}.backup ]  # there should be no backup
  [ -L ${LINK_NAME} ]  # only the overwritten link
  [ $(cat ${LINK_NAME}) = ${TARGET_CONTENT} ]
}

@test "link exist, skip policy specified" {
  echo ${OTHER_FILE_CONTENT} > ${LINK_NAME}
  run link_file ${LINK_TARGET} ${LINK_NAME} 's'
  [ $status -eq 0 ]
  [ ${lines[0]} = 'skipped' ]
  [ -f ${LINK_NAME} ]  # the file still exists..
  [ $(cat ${LINK_NAME}) = ${OTHER_FILE_CONTENT} ]
  [ ! -L ${LINK_NAME} ]  # ..not the link
}
