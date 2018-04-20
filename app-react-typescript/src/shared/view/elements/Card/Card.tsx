import * as React from 'react';
import { Panel } from 'react-bootstrap';

interface IProps {
  children?: React.ReactNode;
}

function Card({ children }: IProps) {
  return (
    <Panel header={children} />
  );
}

export { IProps };
export default Card;
