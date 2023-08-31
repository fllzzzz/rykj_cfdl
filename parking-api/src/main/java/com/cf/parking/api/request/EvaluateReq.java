package com.cf.parking.api.request;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author whx
 * @date 2022/10/21
 */
@Data
@Accessors(chain = true)
public class EvaluateReq {
    private Long parkingOrderId;

    private Integer level;

    private String EvaluateDesc;
}
