import * as React from 'react';
import * as block from 'bem-cn';
import Card from 'shared/view/elements/Card/Card';
import { ListGroup, ListGroupItem, Button, Label } from 'react-bootstrap';
import { IPoll } from 'shared/types/app';
import './PollItem.styl';

interface IProps extends IPoll {
  onOpenClick?: (id: string) => void;
  onEditClick?: (id: string) => void;
}

const b = block('poll-item');

function PollItem({
    id, title, description, isActive, endTime, startTime,
    onOpenClick = new Function(), onEditClick = new Function(),
}: IProps) {
  return (
    <Card>
      <article className={b()}>
        <h3>
          <span>{title}</span>
          <Label className={b('status')()} bsStyle={isActive ? 'success' : 'danger'}>
            {isActive ? 'Active' : 'Inactive'}
          </Label>
        </h3>
        <p>{description}</p>
        <ListGroup>
          <ListGroupItem>
            <span><strong>Start time: </strong></span>
            <span>{startTime}</span>
          </ListGroupItem>
          <ListGroupItem>
            <span><strong>End time: </strong></span>
            <span>{endTime}</span>
          </ListGroupItem>
        </ListGroup>
        <Button
          disabled={!isActive}
          className={b('action')()}
          onClick={onOpenClick.bind(null, id)}
          bsStyle="primary"
        >
          Open
        </Button>
        <Button className={b('action')()} onClick={onEditClick.bind(null, id)} bsStyle="success">
          Edit
        </Button>
      </article>
    </Card>
  );
}

export { IProps };
export default PollItem;
