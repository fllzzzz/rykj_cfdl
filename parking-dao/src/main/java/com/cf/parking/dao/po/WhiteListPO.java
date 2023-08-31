package com.cf.parking.dao.po;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author whx
 * @date 2023-03-30 11:02:41
 * @description 白名单记录表
 */
@Data
@TableName("white_list")
@Accessors(chain = true)
public class WhiteListPO {

	/**
	 * 白名单id
	 */
	@TableId(value = "white_list_id", type = IdType.INPUT)
	private Long whiteListId;

	/**
	 * 车牌号
	 */
	private String plateNo;

	/**
	 * 是否删除，0：未删除  1：已删除
	 */
	private Integer isDelete;

	/**
	 * 创建时间
	 */
	private Date createTm;

	/**
	 * 更新时间
	 */
	private Date updateTm;
}
