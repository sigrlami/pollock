import * as React from 'react';
import * as Select from 'react-select';
import 'react-select/dist/react-select.min.css';
import './SelectInput.styl';

type Props = Select.ReactSelectProps;
type Option = Select.Option;

function SelectInput(props: Props) {
  return (
    <Select {...props} />
  );
}

export { Props, Option };
export default SelectInput;
